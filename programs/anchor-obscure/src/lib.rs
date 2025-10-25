use anchor_lang::prelude::*;
use anchor_spl::token::{self, Mint, Token, TokenAccount, Transfer, MintTo, Burn};
use anchor_lang::solana_program::clock::Clock;

declare_id!("5VpNKsjLNdkvh8x1tdcR2Y1Fft9457SVfsvtkvTLqwGb");

pub const BASIS_POINTS: u64 = 10_000;
pub const SCALE_1E12: u128 = 1_000_000_000_000;
pub const SECONDS_PER_YEAR: u64 = 31_536_000; // 365d
// On Solana, assume ~2 slots/sec => ~63,072,000 slots per year. We compute inline.

// Macro to generate market signer seeds
macro_rules! market_seeds {
    ($market:expr) => {
        [
            b"market_signer".as_ref(),
            $market.key().as_ref(),
        ]
    };
}

#[program]
pub mod isolated_lending {
    use super::*;

    pub fn init_controller(ctx: Context<InitController>, admin: Pubkey) -> Result<()> {
        let c = &mut ctx.accounts.controller;
        c.authority = admin;
        c.bump = ctx.bumps.controller;
        c.paused = false;
        Ok(())
    }

    pub fn market_init(
        ctx: Context<MarketInit>,
        params: MarketParams,
    ) -> Result<()> {
        let m = &mut ctx.accounts.market;
        require!(!ctx.accounts.controller.paused, ErrorCode::Paused);

        m.controller = ctx.accounts.controller.key();
        m.bump = ctx.bumps.market;
        m.collateral_mint = ctx.accounts.collateral_mint.key();
        m.borrow_mint = ctx.accounts.borrow_mint.key();
        m.c_token_mint = ctx.accounts.c_token_mint.key();
        m.vault_collateral = ctx.accounts.vault_collateral.key();
        m.vault_borrow_liquidity = ctx.accounts.vault_borrow_liquidity.key();
        m.oracle = ctx.accounts.oracle.key();

        // Core params
        require!(params.reserve_factor_bps <= 2000, ErrorCode::InvalidConfig); // e.g., <=20%
        require!(params.collateral_factor_bps <= params.liquidation_threshold_bps, ErrorCode::InvalidConfig);
        require!(params.kink_utilization_bps <= BASIS_POINTS, ErrorCode::InvalidConfig);
        require!(params.close_factor_bps <= BASIS_POINTS, ErrorCode::InvalidConfig);

        m.reserve_factor_bps = params.reserve_factor_bps;
        m.collateral_factor_bps = params.collateral_factor_bps;
        m.liquidation_threshold_bps = params.liquidation_threshold_bps;
        m.base_rate_bps = params.base_rate_bps;
        m.slope1_bps = params.slope1_bps;
        m.slope2_bps = params.slope2_bps;
        m.kink_utilization_bps = params.kink_utilization_bps;
        m.close_factor_bps = params.close_factor_bps;

        m.mint_decimals = ctx.accounts.borrow_mint.decimals;

        // Initial state
        m.last_update_slot = Clock::get()?.slot;
        m.borrow_index = SCALE_1E12;

        // Totals
        m.total_c_tokens = 0;
        m.total_borrows = 0;
        m.total_reserves = 0;

        m.utilization_bps = 0;
        m.borrow_rate_bps = 0;
        m.supply_rate_bps = 0;

        Ok(())
    }

    // LENDERS: deposit borrow token into liquidity vault to earn supply yield, cannot use as collateral
    pub fn lender_deposit(
        ctx: Context<LenderDeposit>,
        amount: u64,
    ) -> Result<()> {
        require!(amount > 0, ErrorCode::InvalidAmount);
        let market = &mut ctx.accounts.market;
        update_interest(market)?;

        // Get current vault cash before deposit
        let vault_cash = ctx.accounts.vault_borrow_liquidity.amount;
        
        // Calculate cTokens to mint based on exchange rate
        let er = c_token_exchange_rate(market, vault_cash)?;
        // c_tokens = amount * SCALE_1E12 / er
        let c_tokens = (amount as u128)
            .checked_mul(SCALE_1E12).ok_or(ErrorCode::MathError)?
            .checked_div(er).ok_or(ErrorCode::MathError)? as u64;

        require!(c_tokens > 0, ErrorCode::RoundingTooSmall);

        // Transfer underlying tokens in
        token::transfer(
            CpiContext::new(
                ctx.accounts.token_program.to_account_info(),
                Transfer {
                    from: ctx.accounts.from_lender_liquidity.to_account_info(),
                    to: ctx.accounts.vault_borrow_liquidity.to_account_info(),
                    authority: ctx.accounts.lender.to_account_info(),
                },
            ),
            amount,
        )?;

        // Mint cTokens to lender
        let market_key = market.key();
        let seeds = &[
            b"market_signer".as_ref(),
            market_key.as_ref(),
            &[market.bump],
        ];
        let signer = &[&seeds[..]];

        token::mint_to(
            CpiContext::new_with_signer(
                ctx.accounts.token_program.to_account_info(),
                MintTo {
                    mint: ctx.accounts.c_token_mint.to_account_info(),
                    to: ctx.accounts.lender_c_token_account.to_account_info(),
                    authority: ctx.accounts.market_signer.to_account_info(),
                },
                signer,
            ),
            c_tokens,
        )?;

        market.total_c_tokens = market.total_c_tokens.checked_add(c_tokens).ok_or(ErrorCode::MathError)?;
        Ok(())
    }

    // LENDERS: withdraw liquidity by redeeming cTokens
    pub fn lender_withdraw(
        ctx: Context<LenderWithdraw>,
        c_tokens: u64,
    ) -> Result<()> {
        require!(c_tokens > 0, ErrorCode::InvalidAmount);
        let market = &mut ctx.accounts.market;
        update_interest(market)?;

        // Get current vault cash
        let vault_cash = ctx.accounts.vault_borrow_liquidity.amount;
        
        let er = c_token_exchange_rate(market, vault_cash)?;
        // underlying = c_tokens * er / SCALE_1E12
        let amount = (c_tokens as u128)
            .checked_mul(er).ok_or(ErrorCode::MathError)?
            .checked_div(SCALE_1E12).ok_or(ErrorCode::MathError)? as u64;

        // Ensure enough liquidity in vault
        require!(amount <= vault_cash, ErrorCode::InsufficientLiquidity);

        // Burn cTokens from lender
        token::burn(
            CpiContext::new(
                ctx.accounts.token_program.to_account_info(),
                Burn {
                    mint: ctx.accounts.c_token_mint.to_account_info(),
                    from: ctx.accounts.lender_c_token_account.to_account_info(),
                    authority: ctx.accounts.lender.to_account_info(),
                },
            ),
            c_tokens,
        )?;

        market.total_c_tokens = market.total_c_tokens.checked_sub(c_tokens).ok_or(ErrorCode::MathError)?;

        // Transfer underlying tokens out
        let market_key = market.key();
        let seeds = &[
            b"market_signer".as_ref(),
            market_key.as_ref(),
            &[market.bump],
        ];
        let signer = &[&seeds[..]];

        token::transfer(
            CpiContext::new_with_signer(
                ctx.accounts.token_program.to_account_info(),
                Transfer {
                    from: ctx.accounts.vault_borrow_liquidity.to_account_info(),
                    to: ctx.accounts.to_lender_liquidity.to_account_info(),
                    authority: ctx.accounts.market_signer.to_account_info(),
                },
                signer,
            ),
            amount,
        )?;

        Ok(())
    }

    // BORROWERS: deposit collateral tokens; these can be enabled as collateral
    pub fn borrower_deposit(
        ctx: Context<BorrowerDeposit>,
        amount: u64,
        enable_collateral: bool,
    ) -> Result<()> {
        require!(amount > 0, ErrorCode::InvalidAmount);
        let market = &mut ctx.accounts.market;
        update_interest(market)?;

        // Move collateral into market vault
        token::transfer(
            CpiContext::new(
                ctx.accounts.token_program.to_account_info(),
                Transfer {
                    from: ctx.accounts.from_user_collateral.to_account_info(),
                    to: ctx.accounts.vault_collateral.to_account_info(),
                    authority: ctx.accounts.user.to_account_info(),
                },
            ),
            amount,
        )?;

        let p = &mut ctx.accounts.user_position;
        init_user_position_if_needed(p, market, &ctx.accounts.user.key())?;
        p.supply_shares = p.supply_shares.checked_add(amount).ok_or(ErrorCode::MathError)?; // 1:1 shares for collateral
        if enable_collateral {
            p.collateral_enabled = true;
        }
        Ok(())
    }

    // BORROWERS: withdraw collateral if still healthy; cannot withdraw lender liquidity from this flow
    pub fn borrower_withdraw<'info>(
        ctx: Context<'_, '_, 'info, 'info, BorrowerWithdraw<'info>>,
        amount: u64,
    ) -> Result<()> {
        require!(amount > 0, ErrorCode::InvalidAmount);
        let market = &mut ctx.accounts.market;
        update_interest(market)?;

        let p = &mut ctx.accounts.user_position;
        require_keys_eq!(p.market, market.key(), ErrorCode::AccountMismatch);
        require_keys_eq!(p.user, ctx.accounts.user.key(), ErrorCode::AccountMismatch);
        require!(p.supply_shares >= amount, ErrorCode::InsufficientShares);

        // Hypothetical redeem: check LTV and health using aggregate across all markets passed
        {
            let controller = &ctx.accounts.controller;
            let remaining = ctx.remaining_accounts;
            enforce_borrow_power_after_hypo_redeem(
                controller,
                remaining,
                (market.key(), amount, amount),
                ctx.accounts.collateral_mint.decimals,
            )?;
            enforce_health_after_hypo_redeem(
                controller,
                remaining,
                (market.key(), amount, amount),
                ctx.accounts.collateral_mint.decimals,
            )?;
        }

        // Transfer collateral back to user
        token::transfer(
            CpiContext::new_with_signer(
                ctx.accounts.token_program.to_account_info(),
                Transfer {
                    from: ctx.accounts.vault_collateral.to_account_info(),
                    to: ctx.accounts.to_user_collateral.to_account_info(),
                    authority: ctx.accounts.market_signer.to_account_info(),
                },
                &[&market_seeds!(market)],
            ),
            amount,
        )?;

        p.supply_shares = p.supply_shares.checked_sub(amount).ok_or(ErrorCode::MathError)?;
        Ok(())
    }

    // BORROWERS: take borrow token loan using collateral; interest accrues over time
    pub fn borrower_borrow<'info>(
        ctx: Context<'_, '_, 'info, 'info, BorrowerBorrow<'info>>,
        amount: u64,
    ) -> Result<()> {
        require!(amount > 0, ErrorCode::InvalidAmount);
        let market = &mut ctx.accounts.market;
        update_interest(market)?;

        let p = &mut ctx.accounts.user_position;
        require!(p.collateral_enabled, ErrorCode::CollateralDisabled);
        require_keys_eq!(p.market, market.key(), ErrorCode::AccountMismatch);
        require_keys_eq!(p.user, ctx.accounts.user.key(), ErrorCode::AccountMismatch);

        // Ensure liquidity available
        require!(ctx.accounts.vault_borrow_liquidity.amount >= amount, ErrorCode::InsufficientLiquidity);

        // Hypothetical new borrow: check borrow power and health
        {
            let controller = &ctx.accounts.controller;
            let remaining = ctx.remaining_accounts;
            enforce_borrow_power_after_hypo_borrow(
                controller,
                remaining,
                (market.key(), amount),
                ctx.accounts.borrow_mint.decimals,
            )?;
            enforce_health_after_hypo_borrow(
                controller,
                remaining,
                (market.key(), amount),
                ctx.accounts.borrow_mint.decimals,
            )?;
        }

        // Update user debt on index
        if p.last_borrow_index == 0 {
            p.last_borrow_index = market.borrow_index;
        }
        let current_debt = accrue_debt(p, market)?;
        // new principal = current_debt + amount re-normalized to principal scale at current index
        let new_debt = current_debt
            .checked_add(amount as u128).ok_or(ErrorCode::MathError)?;
        let new_principal = (new_debt
            .checked_mul(SCALE_1E12).ok_or(ErrorCode::MathError)?)
            .checked_div(market.borrow_index).ok_or(ErrorCode::MathError)? as u64;

        p.borrowed_principal = new_principal;
        p.last_borrow_index = market.borrow_index;
        market.total_borrows = market.total_borrows.checked_add(amount).ok_or(ErrorCode::MathError)?;

        // Transfer borrow tokens to user
        token::transfer(
            CpiContext::new_with_signer(
                ctx.accounts.token_program.to_account_info(),
                Transfer {
                    from: ctx.accounts.vault_borrow_liquidity.to_account_info(),
                    to: ctx.accounts.to_user_borrow_tokens.to_account_info(),
                    authority: ctx.accounts.market_signer.to_account_info(),
                },
                &[&market_seeds!(market)],
            ),
            amount,
        )?;
        Ok(())
    }

    // BORROWERS: repay debt in borrow tokens
    pub fn borrower_repay(
        ctx: Context<BorrowerRepay>,
        repay_amount: u64,
    ) -> Result<()> {
        require!(repay_amount > 0, ErrorCode::InvalidAmount);
        let market = &mut ctx.accounts.market;
        update_interest(market)?;

        let p = &mut ctx.accounts.user_position;
        require_keys_eq!(p.market, market.key(), ErrorCode::AccountMismatch);
        require_keys_eq!(p.user, ctx.accounts.user.key(), ErrorCode::AccountMismatch);

        let current_debt = accrue_debt(p, market)? as u64;
        let pay = repay_amount.min(current_debt);

        // Transfer from user to vault liquidity
        token::transfer(
            CpiContext::new(
                ctx.accounts.token_program.to_account_info(),
                Transfer {
                    from: ctx.accounts.from_user_borrow_tokens.to_account_info(),
                    to: ctx.accounts.vault_borrow_liquidity.to_account_info(),
                    authority: ctx.accounts.user.to_account_info(),
                },
            ),
            pay,
        )?;

        // Update principal
        let remaining = current_debt.checked_sub(pay).ok_or(ErrorCode::MathError)?;
        if remaining == 0 {
            p.borrowed_principal = 0;
            p.last_borrow_index = 0;
        } else {
            // re-normalize remaining to principal at current index
            let new_principal = (remaining as u128)
                .checked_mul(SCALE_1E12).ok_or(ErrorCode::MathError)?
                .checked_div(market.borrow_index).ok_or(ErrorCode::MathError)? as u64;
            p.borrowed_principal = new_principal;
            p.last_borrow_index = market.borrow_index;
        }

        market.total_borrows = market.total_borrows.checked_sub(pay).ok_or(ErrorCode::MathError)?;
        Ok(())
    }

    // LIQUIDATION: repay borrow on behalf of unhealthy user, seize collateral with incentive
    pub fn liquidate<'info>(
        ctx: Context<'_, '_, 'info, 'info, Liquidate<'info>>,
        repay_amount_requested: u64,
        // price feeds provided in remaining accounts: triplets (market, user_position, oracle) for all registry markets
    ) -> Result<()> {
        require!(repay_amount_requested > 0, ErrorCode::InvalidAmount);

        let controller = &ctx.accounts.controller;
        let registry = &ctx.accounts.registry;
        let market = &mut ctx.accounts.market; // the borrow market of the user
        update_interest(market)?;

        // Ensure user is liquidatable in aggregate
        {
            let remaining = ctx.remaining_accounts;
            ensure_liquidatable(controller, registry, remaining)?;
        }

        // Close factor throttles max repay per tx
        let violator = &mut ctx.accounts.violator_position;
        require_keys_eq!(violator.market, market.key(), ErrorCode::AccountMismatch);
        require_keys_eq!(violator.user, ctx.accounts.violator.key(), ErrorCode::AccountMismatch);

        let current_debt = accrue_debt(violator, market)? as u64;
        require!(current_debt > 0, ErrorCode::NoDebt);

        let max_repay = (current_debt as u128)
            .checked_mul(market.close_factor_bps as u128).ok_or(ErrorCode::MathError)?
            .checked_div(BASIS_POINTS as u128).ok_or(ErrorCode::MathError)? as u64;

        let repay_amount = repay_amount_requested.min(max_repay).min(current_debt);
        require!(repay_amount > 0, ErrorCode::InvalidAmount);

        // Transfer repay from liquidator to vault
        token::transfer(
            CpiContext::new(
                ctx.accounts.token_program.to_account_info(),
                Transfer {
                    from: ctx.accounts.from_liquidator_borrow_tokens.to_account_info(),
                    to: ctx.accounts.vault_borrow_liquidity.to_account_info(),
                    authority: ctx.accounts.liquidator.to_account_info(),
                },
            ),
            repay_amount,
        )?;

        // Update violator's debt principal
        let remaining = current_debt.checked_sub(repay_amount).ok_or(ErrorCode::MathError)?;
        if remaining == 0 {
            violator.borrowed_principal = 0;
            violator.last_borrow_index = 0;
        } else {
            let new_principal = (remaining as u128)
                .checked_mul(SCALE_1E12).ok_or(ErrorCode::MathError)?
                .checked_div(market.borrow_index).ok_or(ErrorCode::MathError)? as u64;
            violator.borrowed_principal = new_principal;
            violator.last_borrow_index = market.borrow_index;
        }
        market.total_borrows = market.total_borrows.checked_sub(repay_amount).ok_or(ErrorCode::MathError)?;

        // Compute seize of collateral with liquidation incentive
        // For simplicity, we support seizing collateral of the same market (isolated vault pair).
        // Extend if you support cross-asset liquidation baskets.
        let price_usd_borrow = get_price_checked(controller, market, &ctx.accounts.borrow_oracle)?;
        let price_usd_collat = get_price_checked(controller, market, &ctx.accounts.collateral_oracle)?;

        // Convert repay_amount in borrow token USD
        let repay_value_usd = value_usd_from_underlying(
            repay_amount,
            price_usd_borrow,
            ctx.accounts.borrow_mint.decimals,
        )?;

        // liquidation bonus: for example 5% bonus embedded via market.liquidation_bonus_bps if desired.
        // Here, we reuse liquidation_threshold vs collateral_factor. For a classic flow, add a bonus param.
        let bonus_bps: u64 = 500; // 5% example; make it configurable in Market if needed
        let seize_value_usd = repay_value_usd
            .checked_mul((BASIS_POINTS + bonus_bps) as u128).ok_or(ErrorCode::MathError)?
            .checked_div(BASIS_POINTS as u128).ok_or(ErrorCode::MathError)?;

        // Convert seize value to collateral units
        let seize_amount = (seize_value_usd
            .checked_mul(10u128.pow(ctx.accounts.collateral_mint.decimals as u32)).ok_or(ErrorCode::MathError)?)
            .checked_div(price_usd_collat).ok_or(ErrorCode::MathError)? as u64;

        require!(violator.supply_shares >= seize_amount, ErrorCode::InsufficientCollateralToSeize);

        // Move collateral from vault to liquidator
        token::transfer(
            CpiContext::new_with_signer(
                ctx.accounts.token_program.to_account_info(),
                Transfer {
                    from: ctx.accounts.vault_collateral.to_account_info(),
                    to: ctx.accounts.to_liquidator_collateral.to_account_info(),
                    authority: ctx.accounts.market_signer.to_account_info(),
                },
                &[&market_seeds!(market)],
            ),
            seize_amount,
        )?;

        violator.supply_shares = violator.supply_shares.checked_sub(seize_amount).ok_or(ErrorCode::MathError)?;
        Ok(())
    }
}

/**************
 * ACCOUNTS
 **************/

#[account]
pub struct Controller {
    pub authority: Pubkey,
    pub bump: u8,
    pub paused: bool,
}

#[derive(AnchorSerialize, AnchorDeserialize, Clone, Copy)]
pub struct MarketParams {
    pub reserve_factor_bps: u64,
    pub collateral_factor_bps: u64,
    pub liquidation_threshold_bps: u64,
    pub base_rate_bps: u64,
    pub slope1_bps: u64,
    pub slope2_bps: u64,
    pub kink_utilization_bps: u64,
    pub close_factor_bps: u64,
}

#[account]
pub struct Market {
    pub controller: Pubkey,
    pub bump: u8,

    pub collateral_mint: Pubkey,
    pub borrow_mint: Pubkey,
    pub c_token_mint: Pubkey, // cToken mint for lenders

    pub vault_collateral: Pubkey,
    pub vault_borrow_liquidity: Pubkey,

    pub oracle: Pubkey, // placeholder single oracle for both tokens; extend to two if needed

    // interest reserve model
    pub reserve_factor_bps: u64, 
    pub collateral_factor_bps: u64,
    pub liquidation_threshold_bps: u64,

    pub base_rate_bps: u64,
    pub slope1_bps: u64,
    pub slope2_bps: u64,
    pub kink_utilization_bps: u64,
    pub close_factor_bps: u64,

    pub last_update_slot: u64,
    pub borrow_index: u128,
    pub utilization_bps: u64,
    pub borrow_rate_bps: u64,
    pub supply_rate_bps: u64,

    pub total_c_tokens: u64, // total cTokens minted to lenders
    pub total_borrows: u64,  // principal in underlying units
    pub total_reserves: u64,

    pub mint_decimals: u8,
}

#[account]
pub struct UserPosition {
    pub market: Pubkey,
    pub user: Pubkey,

    // For borrowers: supply_shares are raw collateral units (1:1 with collateral token)
    pub supply_shares: u64,

    pub collateral_enabled: bool,

    pub borrowed_principal: u64,
    pub last_borrow_index: u128,
}

#[account]
pub struct UserRegistry {
    pub count: u16,
    pub markets: [Pubkey; 64], // extend as needed
}

/**************
 * CTX
 **************/

#[derive(Accounts)]
pub struct InitController<'info> {
    #[account(
        init,
        payer = payer,
        seeds = [b"controller"],
        bump,
        space = 8 + 32 + 1 + 1
    )]
    pub controller: Account<'info, Controller>,

    #[account(mut)]
    pub payer: Signer<'info>,

    pub system_program: Program<'info, System>,
}

#[derive(Accounts)]
pub struct MarketInit<'info> {
    #[account(mut, has_one = authority)]
    pub controller: Account<'info, Controller>,
    /// CHECK: Admin authority is allowed to init markets
    #[account(mut, address = controller.authority)]
    pub authority: Signer<'info>,

    #[account(
      init,
      payer = authority,
      seeds = [b"market", collateral_mint.key().as_ref(), borrow_mint.key().as_ref()],
      bump,
      space = 8 +  // disc
        32 + 1 + // controller + bump
        32 + 32 + 32 + // mints (including c_token_mint)
        32 + 32 + // vaults
        32 + // oracle
        8*8 + // u64 params
        8 + 16 + // time + borrow_index
        8 + 8 + 8 + // rates
        8 + 8 + 8 + // totals
        1 // decimals
    )]
    pub market: Account<'info, Market>,

    pub collateral_mint: Account<'info, Mint>,
    pub borrow_mint: Account<'info, Mint>,

    #[account(
        init,
        payer = authority,
        mint::decimals = borrow_mint.decimals,
        mint::authority = market_signer
    )]
    pub c_token_mint: Account<'info, Mint>,

    #[account(
        init,
        payer = authority,
        token::mint = collateral_mint,
        token::authority = market_signer
    )]
    pub vault_collateral: Account<'info, TokenAccount>,

    #[account(
        init,
        payer = authority,
        token::mint = borrow_mint,
        token::authority = market_signer
    )]
    pub vault_borrow_liquidity: Account<'info, TokenAccount>,

    /// CHECK: PDA signer for vaults
    #[account(seeds = [b"market_signer", market.key().as_ref()], bump)]
    pub market_signer: UncheckedAccount<'info>,

    /// CHECK: replace with real oracle account (Pyth/Switchboard)
    pub oracle: UncheckedAccount<'info>,

    pub token_program: Program<'info, Token>,
    pub system_program: Program<'info, System>,
    pub rent: Sysvar<'info, Rent>,
}

#[derive(Accounts)]
pub struct LenderDeposit<'info> {
    #[account(mut)]
    pub market: Account<'info, Market>,

    #[account(
        mut,
        token::mint = borrow_mint,
        token::authority = lender
    )]
    pub from_lender_liquidity: Account<'info, TokenAccount>,

    #[account(mut, address = market.vault_borrow_liquidity)]
    pub vault_borrow_liquidity: Account<'info, TokenAccount>,

    #[account(mut, address = market.c_token_mint)]
    pub c_token_mint: Account<'info, Mint>,

    #[account(
        mut,
        token::mint = c_token_mint,
        token::authority = lender
    )]
    pub lender_c_token_account: Account<'info, TokenAccount>,

    /// CHECK: PDA signer for cToken mint
    #[account(seeds = [b"market_signer", market.key().as_ref()], bump)]
    pub market_signer: UncheckedAccount<'info>,

    #[account(mut)]
    pub lender: Signer<'info>,

    pub borrow_mint: Account<'info, Mint>,
    pub token_program: Program<'info, Token>,
}

#[derive(Accounts)]
pub struct LenderWithdraw<'info> {
    #[account(mut)]
    pub market: Account<'info, Market>,

    #[account(
        mut,
        address = market.vault_borrow_liquidity
    )]
    pub vault_borrow_liquidity: Account<'info, TokenAccount>,

    #[account(
        mut,
        token::mint = borrow_mint,
        token::authority = lender
    )]
    pub to_lender_liquidity: Account<'info, TokenAccount>,

    #[account(mut, address = market.c_token_mint)]
    pub c_token_mint: Account<'info, Mint>,

    #[account(
        mut,
        token::mint = c_token_mint,
        token::authority = lender
    )]
    pub lender_c_token_account: Account<'info, TokenAccount>,

    /// CHECK: PDA signer for vault
    #[account(seeds = [b"market_signer", market.key().as_ref()], bump)]
    pub market_signer: UncheckedAccount<'info>,

    #[account(mut)]
    pub lender: Signer<'info>,

    pub borrow_mint: Account<'info, Mint>,
    pub token_program: Program<'info, Token>,
}

#[derive(Accounts)]
pub struct BorrowerDeposit<'info> {
    #[account(mut)]
    pub market: Account<'info, Market>,

    #[account(
        mut,
        token::mint = collateral_mint,
        token::authority = user
    )]
    pub from_user_collateral: Account<'info, TokenAccount>,

    #[account(mut, address = market.vault_collateral)]
    pub vault_collateral: Account<'info, TokenAccount>,

    #[account(
        init_if_needed,
        payer = user,
        seeds = [b"user_position", market.key().as_ref(), user.key().as_ref()],
        bump,
        space = 8 + 32 + 32 + 8 + 1 + 8 + 16
    )]
    pub user_position: Account<'info, UserPosition>,

    #[account(mut)]
    pub user: Signer<'info>,

    pub collateral_mint: Account<'info, Mint>,

    pub token_program: Program<'info, Token>,
    pub system_program: Program<'info, System>,
}

#[derive(Accounts)]
pub struct BorrowerWithdraw<'info> {
    pub controller: Account<'info, Controller>,

    #[account(mut)]
    pub market: Account<'info, Market>,

    #[account(
        mut,
        address = market.vault_collateral
    )]
    pub vault_collateral: Account<'info, TokenAccount>,

    #[account(
        mut,
        token::mint = collateral_mint,
        token::authority = user
    )]
    pub to_user_collateral: Account<'info, TokenAccount>,

    #[account(
        mut,
        seeds = [b"user_position", market.key().as_ref(), user.key().as_ref()],
        bump
    )]
    pub user_position: Account<'info, UserPosition>,

    /// CHECK: PDA signer for vault
    #[account(seeds = [b"market_signer", market.key().as_ref()], bump)]
    pub market_signer: UncheckedAccount<'info>,

    #[account(mut)]
    pub user: Signer<'info>,

    pub collateral_mint: Account<'info, Mint>,
    pub token_program: Program<'info, Token>,
}

#[derive(Accounts)]
pub struct BorrowerBorrow<'info> {
    pub controller: Account<'info, Controller>,

    #[account(mut)]
    pub market: Account<'info, Market>,

    #[account(
        mut,
        address = market.vault_borrow_liquidity
    )]
    pub vault_borrow_liquidity: Account<'info, TokenAccount>,

    #[account(
        mut,
        token::mint = borrow_mint,
        token::authority = user
    )]
    pub to_user_borrow_tokens: Account<'info, TokenAccount>,

    #[account(
        mut,
        seeds = [b"user_position", market.key().as_ref(), user.key().as_ref()],
        bump
    )]
    pub user_position: Account<'info, UserPosition>,

    /// CHECK: PDA signer for vault
    #[account(seeds = [b"market_signer", market.key().as_ref()], bump)]
    pub market_signer: UncheckedAccount<'info>,

    #[account(mut)]
    pub user: Signer<'info>,

    pub borrow_mint: Account<'info, Mint>,

    pub token_program: Program<'info, Token>,
}

#[derive(Accounts)]
pub struct BorrowerRepay<'info> {
    #[account(mut)]
    pub market: Account<'info, Market>,

    #[account(
        mut,
        address = market.vault_borrow_liquidity
    )]
    pub vault_borrow_liquidity: Account<'info, TokenAccount>,

    #[account(
        mut,
        token::mint = borrow_mint,
        token::authority = user
    )]
    pub from_user_borrow_tokens: Account<'info, TokenAccount>,

    #[account(
        mut,
        seeds = [b"user_position", market.key().as_ref(), user.key().as_ref()],
        bump
    )]
    pub user_position: Account<'info, UserPosition>,

    #[account(mut)]
    pub user: Signer<'info>,

    pub borrow_mint: Account<'info, Mint>,

    pub token_program: Program<'info, Token>,
}

#[derive(Accounts)]
pub struct Liquidate<'info> {
    pub controller: Account<'info, Controller>,
    pub registry: Account<'info, UserRegistry>,

    #[account(mut)]
    pub market: Account<'info, Market>,

    #[account(
        mut,
        address = market.vault_borrow_liquidity
    )]
    pub vault_borrow_liquidity: Account<'info, TokenAccount>,

    #[account(
        mut,
        address = market.vault_collateral
    )]
    pub vault_collateral: Account<'info, TokenAccount>,

    #[account(
        mut,
        token::mint = borrow_mint,
        token::authority = liquidator
    )]
    pub from_liquidator_borrow_tokens: Account<'info, TokenAccount>,

    #[account(
        mut,
        token::mint = collateral_mint,
        token::authority = liquidator
    )]
    pub to_liquidator_collateral: Account<'info, TokenAccount>,

    #[account(
        mut,
        seeds = [b"user_position", market.key().as_ref(), violator.key().as_ref()],
        bump
    )]
    pub violator_position: Account<'info, UserPosition>,

    /// CHECK: PDA signer
    #[account(seeds = [b"market_signer", market.key().as_ref()], bump)]
    pub market_signer: UncheckedAccount<'info>,

    #[account(mut)]
    pub liquidator: Signer<'info>,
    /// CHECK: violator is plain key
    pub violator: UncheckedAccount<'info>,

    pub borrow_mint: Account<'info, Mint>,
    pub collateral_mint: Account<'info, Mint>,

    /// CHECK: placeholder oracle accounts; replace with proper types
    pub borrow_oracle: UncheckedAccount<'info>,
    pub collateral_oracle: UncheckedAccount<'info>,

    pub token_program: Program<'info, Token>,
}

/**************
 * HELPERS & MATH (from your snippet, slightly adapted)
 **************/

fn init_user_position_if_needed(p: &mut Account<UserPosition>, m: &Account<Market>, user: &Pubkey) -> Result<()> {
    if p.market == Pubkey::default() {
        p.market = m.key();
        p.user = *user;
        p.supply_shares = 0;
        p.collateral_enabled = false;
        p.borrowed_principal = 0;
        p.last_borrow_index = 0;
    }
    Ok(())
}

fn update_interest(m: &mut Account<Market>) -> Result<()> {
    let now_slot = Clock::get()?.slot;
    let slots = now_slot.saturating_sub(m.last_update_slot);
    if slots == 0 {
        return Ok(());
    }

    // Calculate utilization based on cToken model
    // We need to estimate total cash. Since we track total_c_tokens and know borrows/reserves,
    // we can derive this. However, for simplicity in interest updates, we use:
    // utilization = borrows / (total_underlying_assets)
    // where total_underlying ≈ equivalent value of all cTokens if fully redeemed
    
    let borrows = m.total_borrows as u128;
    let reserves = m.total_reserves as u128;
    
    // Approximate total assets for utilization: if we have cTokens outstanding,
    // the total assets backing them = (cTokens * current_value) / SCALE
    // But this creates circular dependency. Instead use: total = borrows + (implied cash - reserves)
    // For a more accurate approach, we'd pass vault_cash, but for now use a conservative estimate
    
    // Simple approach: utilization = borrows / max(borrows + some_buffer, 1)
    // Better: track this properly by requiring vault account in update calls
    // For now, use the approach that total supply ~= value that backs cTokens
    // We'll approximate: if total_c_tokens > 0, then there's liquidity
    
    // Conservative utilization calculation:
    // If borrows exist, we assume utilization based on borrow vs theoretical max capacity
    let util_bps = if borrows == 0 {
        0
    } else if m.total_c_tokens == 0 {
        // No lenders, high utilization
        BASIS_POINTS
    } else {
        // Estimate: we assume total underlying ≈ borrows + reserves (simplified)
        // In reality this should be: cash + borrows - reserves
        // TODO: Consider passing vault_cash to update_interest for accuracy
        let estimated_total = borrows.checked_add(reserves).ok_or(ErrorCode::MathError)?;
        if estimated_total == 0 {
            0
        } else {
            let util = borrows
                .checked_mul(BASIS_POINTS as u128).ok_or(ErrorCode::MathError)?
                .checked_div(estimated_total).ok_or(ErrorCode::MathError)? as u64;
            util.min(BASIS_POINTS)
        }
    };
    m.utilization_bps = util_bps;

    // Kinked borrow rate
    let borrow_bps = if util_bps <= m.kink_utilization_bps {
        m.base_rate_bps
            .checked_add(
                ((util_bps as u128)
                    .checked_mul(m.slope1_bps as u128)
                    .ok_or(ErrorCode::MathError)?)
                    .checked_div(BASIS_POINTS as u128)
                    .ok_or(ErrorCode::MathError)? as u64,
            )
            .ok_or(ErrorCode::MathError)?
    } else {
        let pre = ((m.kink_utilization_bps as u128)
            .checked_mul(m.slope1_bps as u128)
            .ok_or(ErrorCode::MathError)?)
            .checked_div(BASIS_POINTS as u128)
            .ok_or(ErrorCode::MathError)? as u64;
        let excess = (util_bps - m.kink_utilization_bps) as u128;
        let post = excess
            .checked_mul(m.slope2_bps as u128)
            .ok_or(ErrorCode::MathError)?
            .checked_div(BASIS_POINTS as u128)
            .ok_or(ErrorCode::MathError)? as u64;
        m.base_rate_bps
            .checked_add(pre)
            .and_then(|v| v.checked_add(post))
            .ok_or(ErrorCode::MathError)?
    };
    m.borrow_rate_bps = borrow_bps;

    // Supply rate = borrow_rate * utilization * (1 - reserve_factor)
    let supply_bps = (borrow_bps as u128)
        .checked_mul(util_bps as u128).ok_or(ErrorCode::MathError)?
        .checked_div(BASIS_POINTS as u128).ok_or(ErrorCode::MathError)?
        .checked_mul((BASIS_POINTS - m.reserve_factor_bps) as u128).ok_or(ErrorCode::MathError)?
        .checked_div(BASIS_POINTS as u128).ok_or(ErrorCode::MathError)? as u64;
    m.supply_rate_bps = supply_bps;

    // Per-slot scaled increments
    let slots_per_year = (SECONDS_PER_YEAR as u128).checked_mul(2).ok_or(ErrorCode::MathError)?;
    let r_borrow_scaled_per_slot = (borrow_bps as u128)
        .checked_mul(SCALE_1E12).ok_or(ErrorCode::MathError)?
        .checked_div(slots_per_year).ok_or(ErrorCode::MathError)?
        .checked_div(BASIS_POINTS as u128).ok_or(ErrorCode::MathError)?;

    let borrow_incr_scaled = r_borrow_scaled_per_slot
        .checked_mul(slots as u128).ok_or(ErrorCode::MathError)?;

    m.borrow_index = m.borrow_index.checked_add(
        m.borrow_index
            .checked_mul(borrow_incr_scaled).ok_or(ErrorCode::MathError)?
            .checked_div(SCALE_1E12).ok_or(ErrorCode::MathError)?,
    ).ok_or(ErrorCode::MathError)?;

    // Interest accrual on borrows
    // Calculate interest: total_borrows * borrow_rate * time
    let interest_on_borrows = (m.total_borrows as u128)
        .checked_mul(borrow_incr_scaled).ok_or(ErrorCode::MathError)?
        .checked_div(SCALE_1E12).ok_or(ErrorCode::MathError)? as u64;
    
    // IMPORTANT: Add the accrued interest to total_borrows
    // This ensures total_borrows always reflects current debt including interest
    if interest_on_borrows > 0 {
        m.total_borrows = m.total_borrows.checked_add(interest_on_borrows).ok_or(ErrorCode::MathError)?;
        
        // Reserve portion: interest * reserve_factor goes to protocol reserves
        let to_reserves = (interest_on_borrows as u128)
            .checked_mul(m.reserve_factor_bps as u128).ok_or(ErrorCode::MathError)?
            .checked_div(BASIS_POINTS as u128).ok_or(ErrorCode::MathError)? as u64;
        
        if to_reserves > 0 {
            m.total_reserves = m.total_reserves.checked_add(to_reserves).ok_or(ErrorCode::MathError)?;
        }
        
        // Note: The remaining interest (interest - reserves) automatically accrues to lenders
        // because it increases total_borrows without increasing reserves,
        // thus increasing the cToken exchange rate
    }

    m.last_update_slot = now_slot;
    Ok(())
}

// Calculate cToken exchange rate: (cash + borrows - reserves) / total_c_tokens
// Returns exchange rate scaled by SCALE_1E12
fn c_token_exchange_rate(m: &Market, vault_cash: u64) -> Result<u128> {
    if m.total_c_tokens == 0 {
        return Ok(SCALE_1E12); // Initial rate 1:1
    }
    
    // Total underlying = cash + borrows - reserves
    let total_underlying = (vault_cash as u128)
        .checked_add(m.total_borrows as u128).ok_or(ErrorCode::MathError)?
        .checked_sub(m.total_reserves as u128).ok_or(ErrorCode::MathError)?;
    
    // exchange_rate = total_underlying * SCALE_1E12 / total_c_tokens
    let rate = total_underlying
        .checked_mul(SCALE_1E12).ok_or(ErrorCode::MathError)?
        .checked_div(m.total_c_tokens as u128).ok_or(ErrorCode::MathError)?;
    
    Ok(rate)
}

fn accrue_debt(p: &UserPosition, m: &Market) -> Result<u128> {
    if p.borrowed_principal == 0 || p.last_borrow_index == 0 {
        return Ok(p.borrowed_principal as u128);
    }
    let num = (p.borrowed_principal as u128)
        .checked_mul(m.borrow_index).ok_or(ErrorCode::MathError)?;
    let res = num.checked_div(p.last_borrow_index).ok_or(ErrorCode::MathError)?;
    Ok(res)
}

// Replace with real oracle logic; ensure staleness + conf checks
fn get_price_checked(
    _controller: &Account<Controller>,
    market: &Market,
    oracle_ai: &AccountInfo,
) -> Result<u128> {
    require_keys_eq!(oracle_ai.key(), market.oracle, ErrorCode::InvalidOracle);
    Ok(1_0000_0000u128) // 1e8 scale; replace
}

// Registry validation for liquidation passes: remaining accounts triplets
fn validate_against_registry<'info>(
    registry: &Account<'info, UserRegistry>,
    remaining: &'info [AccountInfo<'info>],
) -> Result<()> {
    require!(remaining.len() % 3 == 0, ErrorCode::AccountMismatch);
    let mut provided: Vec<Pubkey> = Vec::new();
    let mut i = 0;
    while i < remaining.len() {
        let m_ai = &remaining[i];
        let market: Account<Market> =
            Account::try_from(m_ai).map_err(|_| ErrorCode::AccountMismatch)?;
        provided.push(market.key());
        i += 3;
    }
    for idx in 0..registry.count as usize {
        let mk = registry.markets[idx];
        require!(provided.contains(&mk), ErrorCode::RegistryMismatch);
    }
    Ok(())
}

fn value_usd_from_underlying(amount: u64, price: u128, decimals: u8) -> Result<u128> {
    let scale = 10u128.pow(decimals as u32);
    let v = (amount as u128)
        .checked_mul(price).ok_or(ErrorCode::MathError)?
        .checked_div(scale).ok_or(ErrorCode::MathError)?;
    Ok(v)
}

fn aggregate_portfolio_from_remaining<'info>(
    controller: &Account<'info, Controller>,
    remaining: &'info [AccountInfo<'info>],
    hypo_redeem: Option<(Pubkey, u64, u64, u8)>, // (market, shares, underlying, decimals)
    hypo_new_borrow: Option<(Pubkey, u64, u8)>,  // (market, amount, decimals)
    use_ltv: bool,
) -> Result<(u128, u128, Pubkey)> {
    require!(remaining.len() % 3 == 0, ErrorCode::AccountMismatch);
    if remaining.is_empty() {
        return Ok((0, 0, controller.authority));
    }

    let mut total_debt_value: u128 = 0;
    let mut total_collateral_value_limit: u128 = 0;
    let mut inferred_user: Option<Pubkey> = None;

    let mut i = 0;
    while i < remaining.len() {
        let m_ai = &remaining[i];
        let p_ai = &remaining[i + 1];
        let o_ai = &remaining[i + 2];

        let mut market: Account<Market> =
            Account::try_from(m_ai).map_err(|_| ErrorCode::AccountMismatch)?;
        let position: Account<UserPosition> =
            Account::try_from(p_ai).map_err(|_| ErrorCode::AccountMismatch)?;

        require_keys_eq!(position.market, market.key(), ErrorCode::AccountMismatch);

        let user_pk = if let Some(pk) = inferred_user {
            pk
        } else {
            let pk = position.user;
            inferred_user = Some(pk);
            pk
        };
        require_keys_eq!(position.user, user_pk, ErrorCode::AccountMismatch);

        require_keys_eq!(o_ai.key(), market.oracle, ErrorCode::InvalidOracle);

        update_interest(&mut market)?;

        let price_usd = get_price_checked(controller, &market, o_ai)?;

        // Collateral
        if position.collateral_enabled && position.supply_shares > 0 {
            // For collateral, shares are 1:1 with underlying units
            let underlying = position.supply_shares;

            let value_usd = value_usd_from_underlying(underlying, price_usd, market.mint_decimals)?;
            let factor_bps = if use_ltv {
                market.collateral_factor_bps
            } else {
                market.liquidation_threshold_bps
            } as u128;

            let allowed = value_usd
                .checked_mul(factor_bps).ok_or(ErrorCode::MathError)?
                .checked_div(BASIS_POINTS as u128).ok_or(ErrorCode::MathError)?;
            total_collateral_value_limit = total_collateral_value_limit
                .checked_add(allowed).ok_or(ErrorCode::MathError)?;
        }

        // Debt
        let accrued = accrue_debt(&position, &market)?;
        if accrued > 0 {
            let debt_usd = value_usd_from_underlying(accrued as u64, price_usd, market.mint_decimals)?;
            total_debt_value = total_debt_value.checked_add(debt_usd).ok_or(ErrorCode::MathError)?;
        }

        // Hypo redeem
        if let Some((redeem_mkt, _redeem_shares, redeem_underlying, redeem_decimals)) = hypo_redeem {
            if redeem_mkt == market.key() && position.collateral_enabled {
                let redeem_value_usd = value_usd_from_underlying(redeem_underlying, price_usd, redeem_decimals)?;
                let factor_bps = if use_ltv {
                    market.collateral_factor_bps
                } else {
                    market.liquidation_threshold_bps
                } as u128;
                let reduce = redeem_value_usd
                    .checked_mul(factor_bps).ok_or(ErrorCode::MathError)?
                    .checked_div(BASIS_POINTS as u128).ok_or(ErrorCode::MathError)?;
                total_collateral_value_limit = total_collateral_value_limit.saturating_sub(reduce);
            }
        }

        // Hypo new borrow
        if let Some((borrow_mkt, borrow_amount, borrow_decimals)) = hypo_new_borrow {
            if borrow_mkt == market.key() && borrow_amount > 0 {
                let borrow_value_usd = value_usd_from_underlying(borrow_amount, price_usd, borrow_decimals)?;
                total_debt_value = total_debt_value.checked_add(borrow_value_usd).ok_or(ErrorCode::MathError)?;
            }
        }

        i += 3;
    }

    let final_user = inferred_user.unwrap_or(controller.authority);
    Ok((total_debt_value, total_collateral_value_limit, final_user))
}

fn enforce_borrow_power_after_hypo_redeem<'info>(
    controller: &Account<'info, Controller>,
    remaining: &'info [AccountInfo<'info>],
    hypo_redeem: (Pubkey, u64, u64),
    decimals: u8,
) -> Result<()> {
    let (total_debt, total_borrow_power, _user) = aggregate_portfolio_from_remaining(
        controller,
        remaining,
        Some((hypo_redeem.0, hypo_redeem.1, hypo_redeem.2, decimals)),
        None,
        true,
    )?;
    require!(total_debt <= total_borrow_power, ErrorCode::InsufficientCollateral);
    Ok(())
}

fn enforce_health_after_hypo_redeem<'info>(
    controller: &Account<'info, Controller>,
    remaining: &'info [AccountInfo<'info>],
    hypo_redeem: (Pubkey, u64, u64),
    decimals: u8,
) -> Result<()> {
    let (total_debt, total_threshold, _user) = aggregate_portfolio_from_remaining(
        controller,
        remaining,
        Some((hypo_redeem.0, hypo_redeem.1, hypo_redeem.2, decimals)),
        None,
        false,
    )?;
    require!(total_debt <= total_threshold, ErrorCode::Unhealthy);
    Ok(())
}

fn enforce_borrow_power_after_hypo_borrow<'info>(
    controller: &Account<'info, Controller>,
    remaining: &'info [AccountInfo<'info>],
    hypo_borrow: (Pubkey, u64),
    decimals: u8,
) -> Result<()> {
    let (total_debt, total_borrow_power, _user) = aggregate_portfolio_from_remaining(
        controller,
        remaining,
        None,
        Some((hypo_borrow.0, hypo_borrow.1, decimals)),
        true,
    )?;
    require!(total_debt <= total_borrow_power, ErrorCode::InsufficientCollateral);
    Ok(())
}

fn enforce_health_after_hypo_borrow<'info>(
    controller: &Account<'info, Controller>,
    remaining: &'info [AccountInfo<'info>],
    hypo_borrow: (Pubkey, u64),
    decimals: u8,
) -> Result<()> {
    let (total_debt, total_threshold, _user) = aggregate_portfolio_from_remaining(
        controller,
        remaining,
        None,
        Some((hypo_borrow.0, hypo_borrow.1, decimals)),
        false,
    )?;
    require!(total_debt <= total_threshold, ErrorCode::Unhealthy);
    Ok(())
}

fn ensure_liquidatable<'info>(
    controller: &Account<'info, Controller>,
    registry: &Account<'info, UserRegistry>,
    remaining: &'info [AccountInfo<'info>],
) -> Result<()> {
    validate_against_registry(registry, remaining)?;
    let (total_debt, total_threshold, _user) =
        aggregate_portfolio_from_remaining(controller, remaining, None, None, false)?;
    require!(total_debt > total_threshold, ErrorCode::NotLiquidatable);
    Ok(())
}

/**************
 * ERRORS
 **************/
#[error_code]
pub enum ErrorCode {
    #[msg("Math error")]
    MathError,
    #[msg("Invalid oracle account")]
    InvalidOracle,
    #[msg("Invalid config")]
    InvalidConfig,
    #[msg("Paused")]
    Paused,
    #[msg("Account mismatch")]
    AccountMismatch,
    #[msg("Registry mismatch")]
    RegistryMismatch,
    #[msg("Invalid amount")]
    InvalidAmount,
    #[msg("Rounding too small")]
    RoundingTooSmall,
    #[msg("Insufficient liquidity")]
    InsufficientLiquidity,
    #[msg("Insufficient shares")]
    InsufficientShares,
    #[msg("Insufficient collateral")]
    InsufficientCollateral,
    #[msg("Collateral disabled")]
    CollateralDisabled,
    #[msg("Unhealthy position")]
    Unhealthy,
    #[msg("Not liquidatable")]
    NotLiquidatable,
    #[msg("No debt")]
    NoDebt,
    #[msg("Insufficient collateral to seize")]
    InsufficientCollateralToSeize,
}