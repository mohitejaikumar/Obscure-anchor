use anchor_lang::prelude::*;
use anchor_spl::token::{self, Mint, Token, TokenAccount};

declare_id!("5VpNKsjLNdkvh8x1tdcR2Y1Fft9457SVfsvtkvTLqwGb");

// Constants
const BASIS_POINTS: u64 = 10_000;
const SECONDS_PER_YEAR: u64 = 31_536_000;
const SCALE_1E12: u128 = 1_000_000_000_000;

// Errors
#[error_code]
pub enum ErrorCode {
    #[msg("Paused")]
    Paused,
    #[msg("Math error")]
    MathError,
    #[msg("Insufficient balance")]
    InsufficientBalance,
    #[msg("Insufficient collateral (LTV)")]
    InsufficientCollateral,
    #[msg("Unhealthy after operation (HF)")]
    Unhealthy,
    #[msg("Not liquidatable")]
    NotLiquidatable,
    #[msg("Invalid mint")]
    InvalidMint,
    #[msg("Invalid owner")]
    InvalidOwner,
    #[msg("Invalid vault")]
    InvalidVault,
    #[msg("Invalid authority")]
    InvalidAuthority,
    #[msg("Invalid oracle")]
    InvalidOracle,
    #[msg("Account mismatch")]
    AccountMismatch,
    #[msg("Registry mismatch")]
    RegistryMismatch,
}

// Controller: global config
#[account]
pub struct Controller {
    pub authority: Pubkey,
    pub treasury: Pubkey,
    pub protocol_fee_bps: u64,          // for liquidation fee to reserves/treasury (subset of bonus)
    pub close_factor_bps: u64,          // max percent of debt repayable in a single liquidation
    pub total_markets: u64,
    pub paused: bool,
    pub bump: u8,
}

// Optional: per-user registry of enabled collateral markets
#[account]
pub struct UserRegistry {
    pub user: Pubkey,
    pub controller: Pubkey,
    // Fixed-size list for simplicity; you can make this a vector and realloc
    pub markets: [Pubkey; 16],
    pub count: u8,
    pub bump: u8,
}

// Market: one per SPL mint
#[account]
pub struct Market {
    pub controller: Pubkey,
    pub mint: Pubkey,
    pub mint_decimals: u8,
    pub supply_vault: Pubkey,

    // Interest rate model (bps)
    pub base_rate_bps: u64,
    pub slope1_bps: u64,
    pub slope2_bps: u64,
    pub kink_utilization_bps: u64,
    pub reserve_factor_bps: u64, // share of interest to reserves

    // Risk params (bps)
    pub collateral_factor_bps: u64,     // LTV for borrow power
    pub liquidation_threshold_bps: u64, // threshold for liquidation
    pub liquidation_bonus_bps: u64,     // bonus the liquidator gets on seized collateral

    // Accounting
    pub total_supply_shares: u64, // supplier shares (internal cToken)
    pub total_borrows: u64,       // principal in underlying units
    pub total_reserves: u64,      // accumulated reserves in underlying units

    // Indices and rates
    pub supply_index: u128, // 1e12 scale
    pub borrow_index: u128, // 1e12 scale
    pub supply_rate_bps: u64,
    pub borrow_rate_bps: u64,
    pub utilization_bps: u64,
    pub last_update_slot: u64,

    // Oracle
    pub oracle: Pubkey,

    pub bump: u8,
}

// UserPosition: per user per market
#[account]
pub struct UserPosition {
    pub user: Pubkey,
    pub market: Pubkey,

    // Supply side
    pub supply_shares: u64,
    pub last_supply_index: u128,
    pub collateral_enabled: bool,

    // Borrow side
    pub borrowed_principal: u64,
    pub last_borrow_index: u128,
}

// Rate model params
#[derive(AnchorSerialize, AnchorDeserialize, Clone, Default)]
pub struct RateModelParams {
    pub base_rate_bps: u64,
    pub slope1_bps: u64,
    pub slope2_bps: u64,
    pub kink_bps: u64,
    pub reserve_factor_bps: u64,
}

// Market risk params
#[derive(AnchorSerialize, AnchorDeserialize, Clone, Default)]
pub struct MarketRiskParams {
    pub collateral_factor_bps: u64,
    pub liquidation_threshold_bps: u64,
    pub liquidation_bonus_bps: u64,
}

#[event]
pub struct SupplyEvent {
    pub user: Pubkey,
    pub market: Pubkey,
    pub amount: u64,
    pub shares_minted: u64,
}
#[event]
pub struct RedeemEvent {
    pub user: Pubkey,
    pub market: Pubkey,
    pub shares_burned: u64,
    pub amount_returned: u64,
}
#[event]
pub struct BorrowEvent {
    pub user: Pubkey,
    pub market: Pubkey,
    pub amount: u64,
}
#[event]
pub struct RepayEvent {
    pub user: Pubkey,
    pub market: Pubkey,
    pub amount: u64,
}
#[event]
pub struct LiquidateEvent {
    pub liquidator: Pubkey,
    pub user: Pubkey,
    pub debt_market: Pubkey,
    pub collateral_market: Pubkey,
    pub repay_amount: u64,
    pub collateral_seized_shares: u64,
    pub collateral_underlying: u64,
    pub protocol_fee_underlying: u64,
}
#[event]
pub struct RateUpdateEvent {
    pub market: Pubkey,
    pub supply_rate_bps: u64,
    pub borrow_rate_bps: u64,
    pub utilization_bps: u64,
}

#[program]
pub mod anchor_lending {
    use super::*;

    pub fn initialize_controller(
        ctx: Context<InitializeController>,
        protocol_fee_bps: u64,
        close_factor_bps: u64,
        bump: u8,
    ) -> Result<()> {
        let c = &mut ctx.accounts.controller;
        c.authority = ctx.accounts.authority.key();
        c.treasury = ctx.accounts.treasury.key();
        c.protocol_fee_bps = protocol_fee_bps;
        c.close_factor_bps = close_factor_bps;
        c.total_markets = 0;
        c.paused = false;
        c.bump = bump;
        Ok(())
    }

    pub fn create_market(
        ctx: Context<CreateMarket>,
        rate: RateModelParams,
        risk: MarketRiskParams,
        oracle: Pubkey,
        bump: u8,
    ) -> Result<()> {
        let m = &mut ctx.accounts.market;
        let c = &mut ctx.accounts.controller;
        let mint = &ctx.accounts.mint;

        m.controller = c.key();
        m.mint = mint.key();
        m.mint_decimals = mint.decimals;
        m.supply_vault = ctx.accounts.supply_vault.key();

        m.base_rate_bps = rate.base_rate_bps;
        m.slope1_bps = rate.slope1_bps;
        m.slope2_bps = rate.slope2_bps;
        m.kink_utilization_bps = rate.kink_bps;
        m.reserve_factor_bps = rate.reserve_factor_bps;

        m.collateral_factor_bps = risk.collateral_factor_bps;
        m.liquidation_threshold_bps = risk.liquidation_threshold_bps;
        m.liquidation_bonus_bps = risk.liquidation_bonus_bps;

        m.total_supply_shares = 0;
        m.total_borrows = 0;
        m.total_reserves = 0;

        m.supply_index = SCALE_1E12;
        m.borrow_index = SCALE_1E12;
        m.supply_rate_bps = 0;
        m.borrow_rate_bps = 0;
        m.utilization_bps = 0;
        m.last_update_slot = Clock::get()?.slot;
        m.oracle = oracle;
        m.bump = bump;

        c.total_markets = c.total_markets.checked_add(1).ok_or(ErrorCode::MathError)?;
        Ok(())
    }

    pub fn update_params(
        ctx: Context<AdminUpdateMarket>,
        rate: Option<RateModelParams>,
        risk: Option<MarketRiskParams>,
        oracle: Option<Pubkey>,
    ) -> Result<()> {
        let c = &ctx.accounts.controller;
        require_keys_eq!(c.authority, ctx.accounts.authority.key(), ErrorCode::InvalidAuthority);
        let m = &mut ctx.accounts.market;
        if let Some(r) = rate {
            m.base_rate_bps = r.base_rate_bps;
            m.slope1_bps = r.slope1_bps;
            m.slope2_bps = r.slope2_bps;
            m.kink_utilization_bps = r.kink_bps;
            m.reserve_factor_bps = r.reserve_factor_bps;
        }
        if let Some(riskp) = risk {
            m.collateral_factor_bps = riskp.collateral_factor_bps;
            m.liquidation_threshold_bps = riskp.liquidation_threshold_bps;
            m.liquidation_bonus_bps = riskp.liquidation_bonus_bps;
        }
        if let Some(o) = oracle {
            m.oracle = o;
        }
        Ok(())
    }

    pub fn set_paused(ctx: Context<SetPaused>, paused: bool) -> Result<()> {
        let c = &mut ctx.accounts.controller;
        require_keys_eq!(c.authority, ctx.accounts.authority.key(), ErrorCode::InvalidAuthority);
        c.paused = paused;
        Ok(())
    }

    pub fn supply(ctx: Context<Supply>, amount: u64) -> Result<()> {
        let controller = &ctx.accounts.controller;
        require!(!controller.paused, ErrorCode::Paused);

        let m = &mut ctx.accounts.market;
        let p = &mut ctx.accounts.position;

        update_interest(m)?;

        let er = supply_exchange_rate(m)?;
        // shares = amount / exchange_rate
        let shares = ((amount as u128)
            .checked_mul(SCALE_1E12).ok_or(ErrorCode::MathError)?)
            .checked_div(er).ok_or(ErrorCode::MathError)? as u64;

        if p.user == Pubkey::default() {
            p.user = ctx.accounts.user.key();
            p.market = m.key();
        } else {
            require_keys_eq!(p.user, ctx.accounts.user.key(), ErrorCode::AccountMismatch);
            require_keys_eq!(p.market, m.key(), ErrorCode::AccountMismatch);
        }

        p.supply_shares = p.supply_shares.checked_add(shares).ok_or(ErrorCode::MathError)?;
        p.last_supply_index = m.supply_index;

        m.total_supply_shares = m.total_supply_shares.checked_add(shares).ok_or(ErrorCode::MathError)?;

        // Transfer tokens into supply vault
        token::transfer(
            CpiContext::new(
                ctx.accounts.token_program.to_account_info(),
                token::Transfer {
                    from: ctx.accounts.user_ata.to_account_info(),
                    to: ctx.accounts.supply_vault.to_account_info(),
                    authority: ctx.accounts.user.to_account_info(),
                },
            ),
            amount,
        )?;

        emit!(SupplyEvent {
            user: p.user,
            market: m.key(),
            amount,
            shares_minted: shares,
        });
        Ok(())
    }

    pub fn redeem<'info>(
        ctx: Context<'_, '_, 'info, 'info, Redeem<'info>>,
        shares: u64,
    ) -> Result<()> {
        let controller = &ctx.accounts.controller;
        require!(!controller.paused, ErrorCode::Paused);

        let m = &mut ctx.accounts.market;
        let p = &mut ctx.accounts.position;

        update_interest(m)?;
        require!(p.supply_shares >= shares, ErrorCode::InsufficientBalance);

        let er = supply_exchange_rate(m)?;
        let amount = ((shares as u128)
            .checked_mul(er).ok_or(ErrorCode::MathError)?)
            .checked_div(SCALE_1E12).ok_or(ErrorCode::MathError)? as u64;

        // liquidity check: available cash in vault must be >= amount
        require!(ctx.accounts.supply_vault.amount >= amount, ErrorCode::InsufficientBalance);

        // LTV check if collateral enabled: post-redeem borrow power using collateral_factor must cover current debt
        if p.collateral_enabled {
            enforce_borrow_power_after_hypo_redeem(
                controller,
                ctx.remaining_accounts,
                (m.key(), shares, amount),
                m.mint_decimals,
            )?;
            // Also ensure still healthy vs liquidation threshold (optional but recommended)
            enforce_health_after_hypo_redeem(
                controller,
                ctx.remaining_accounts,
                (m.key(), shares, amount),
                m.mint_decimals,
            )?;
        }

        p.supply_shares = p.supply_shares.checked_sub(shares).ok_or(ErrorCode::MathError)?;
        p.last_supply_index = m.supply_index;
        m.total_supply_shares = m.total_supply_shares.checked_sub(shares).ok_or(ErrorCode::MathError)?;

        // Transfer underlying from supply vault to user
        let seeds = &[b"market", m.controller.as_ref(), m.mint.as_ref(), &[m.bump]];
        let signer = &[&seeds[..]];
        token::transfer(
            CpiContext::new_with_signer(
                ctx.accounts.token_program.to_account_info(),
                token::Transfer {
                    from: ctx.accounts.supply_vault.to_account_info(),
                    to: ctx.accounts.user_ata.to_account_info(),
                    authority: m.to_account_info(),
                },
                signer,
            ),
            amount,
        )?;

        emit!(RedeemEvent {
            user: p.user,
            market: m.key(),
            shares_burned: shares,
            amount_returned: amount,
        });
        Ok(())
    }

    pub fn set_collateral<'info>(ctx: Context<'_, '_, 'info, 'info, SetCollateral<'info>>, enable: bool) -> Result<()> {
        let controller = &ctx.accounts.controller;
        require!(!controller.paused, ErrorCode::Paused);

        let p = &mut ctx.accounts.position;
        // If disabling collateral, ensure still healthy and within borrow power after removal
        if p.collateral_enabled && !enable {
            enforce_borrow_power_after_hypo_redeem(
                controller,
                ctx.remaining_accounts,
                // Hypo: remove all collateral from this market from borrow power calculation (simulate redeem of all)
                (ctx.accounts.market.key(), p.supply_shares, 0), // we only use shares flag to subtract value
                ctx.accounts.market.mint_decimals,
            )?;
            enforce_health_after_hypo_redeem(
                controller,
                ctx.remaining_accounts,
                (ctx.accounts.market.key(), p.supply_shares, 0),
                ctx.accounts.market.mint_decimals,
            )?;
        }
        p.collateral_enabled = enable;
        Ok(())
    }

    pub fn borrow<'info>(
        ctx: Context<'_, '_, 'info, 'info, Borrow<'info>>,
        amount: u64,
    ) -> Result<()> {
        let controller = &ctx.accounts.controller;
        require!(!controller.paused, ErrorCode::Paused);

        let m = &mut ctx.accounts.borrow_market;
        let borrower = &mut ctx.accounts.borrower_position;

        update_interest(m)?;

        // liquidity check
        require!(ctx.accounts.supply_vault.amount >= amount, ErrorCode::InsufficientBalance);

        // LTV borrow power check: use collateral_factor across portfolio
        enforce_borrow_power_after_hypo_borrow(
            controller,
            ctx.remaining_accounts,
            (m.key(), amount),
            m.mint_decimals,
        )?;

        // Optional additional health check using liquidation threshold
        enforce_health_after_hypo_borrow(
            controller,
            ctx.remaining_accounts,
            (m.key(), amount),
            m.mint_decimals,
        )?;

        // accrue current debt and add
        let current_debt = accrue_debt(borrower, m)?;
        let new_debt = current_debt
            .checked_add(amount as u128).ok_or(ErrorCode::MathError)? as u64;

        borrower.borrowed_principal = new_debt;
        borrower.last_borrow_index = m.borrow_index;

        m.total_borrows = m.total_borrows.checked_add(amount).ok_or(ErrorCode::MathError)?;

        // Transfer from supply_vault to user
        let seeds = &[b"market", m.controller.as_ref(), m.mint.as_ref(), &[m.bump]];
        let signer = &[&seeds[..]];
        token::transfer(
            CpiContext::new_with_signer(
                ctx.accounts.token_program.to_account_info(),
                token::Transfer {
                    from: ctx.accounts.supply_vault.to_account_info(),
                    to: ctx.accounts.user_borrow_ata.to_account_info(),
                    authority: m.to_account_info(),
                },
                signer,
            ),
            amount,
        )?;

        emit!(BorrowEvent {
            user: borrower.user,
            market: m.key(),
            amount,
        });
        Ok(())
    }

    pub fn repay(ctx: Context<Repay>, amount: u64) -> Result<()> {
        let controller = &ctx.accounts.controller;
        require!(!controller.paused, ErrorCode::Paused);

        let m = &mut ctx.accounts.borrow_market;
        let borrower = &mut ctx.accounts.borrower_position;

        update_interest(m)?;

        let current_debt = accrue_debt(borrower, m)?;
        let pay = core::cmp::min(amount as u128, current_debt) as u64;

        let remaining = current_debt
            .checked_sub(pay as u128).ok_or(ErrorCode::MathError)? as u64;
        borrower.borrowed_principal = remaining;
        borrower.last_borrow_index = m.borrow_index;

        m.total_borrows = m.total_borrows.checked_sub(pay).ok_or(ErrorCode::MathError)?;

        token::transfer(
            CpiContext::new(
                ctx.accounts.token_program.to_account_info(),
                token::Transfer {
                    from: ctx.accounts.user_repay_ata.to_account_info(),
                    to: ctx.accounts.supply_vault.to_account_info(),
                    authority: ctx.accounts.user.to_account_info(),
                },
            ),
            pay,
        )?;

        emit!(RepayEvent {
            user: borrower.user,
            market: m.key(),
            amount: pay,
        });
        Ok(())
    }

    pub fn liquidate<'info>(
        ctx: Context<'_, '_, 'info, 'info, Liquidate<'info>>,
        repay_amount: u64,
    ) -> Result<()> {
        let controller = &ctx.accounts.controller;
        require!(!controller.paused, ErrorCode::Paused);

        let debt_m = &mut ctx.accounts.debt_market;
        let col_m = &mut ctx.accounts.collateral_market;
        let u_debt = &mut ctx.accounts.user_debt_position;
        let u_col = &mut ctx.accounts.user_collateral_position;

        update_interest(debt_m)?;
        update_interest(col_m)?;

        // Check if user is liquidatable (HF < 1 based on liquidation thresholds)
        ensure_liquidatable(
            controller,
            &ctx.accounts.user_registry,
            ctx.remaining_accounts,
        )?;

        // Cap repay to close factor
        let current_debt = accrue_debt(u_debt, debt_m)?;
        let max_repay = ((current_debt as u128)
            .checked_mul(controller.close_factor_bps as u128).ok_or(ErrorCode::MathError)?)
            .checked_div(BASIS_POINTS as u128).ok_or(ErrorCode::MathError)? as u64;
        let repay_capped = repay_amount.min(max_repay).min(current_debt as u64);

        // Get prices (scaled to USD per whole token); normalize by decimals
        let debt_price = get_price_checked(controller, debt_m, &ctx.accounts.debt_oracle)?;
        let col_price = get_price_checked(controller, col_m, &ctx.accounts.collateral_oracle)?;

        // Value in USD: repay_value = repay_units * price / 10^decimals
        let debt_decimals_scale = 10u128.pow(debt_m.mint_decimals as u32);
        let col_decimals_scale = 10u128.pow(col_m.mint_decimals as u32);

        let repay_value_usd = (repay_capped as u128)
            .checked_mul(debt_price).ok_or(ErrorCode::MathError)?
            .checked_div(debt_decimals_scale).ok_or(ErrorCode::MathError)?;

        // Apply liquidation bonus and protocol fee
        let gross_value_usd = repay_value_usd
            .checked_mul((BASIS_POINTS + col_m.liquidation_bonus_bps) as u128)
            .ok_or(ErrorCode::MathError)?
            .checked_div(BASIS_POINTS as u128)
            .ok_or(ErrorCode::MathError)?;

        let protocol_fee_usd = gross_value_usd
            .checked_mul(controller.protocol_fee_bps as u128)
            .ok_or(ErrorCode::MathError)?
            .checked_div(BASIS_POINTS as u128)
            .ok_or(ErrorCode::MathError)?;

        let net_value_usd = gross_value_usd
            .checked_sub(protocol_fee_usd).ok_or(ErrorCode::MathError)?;

        // Convert USD value to collateral units underlying to seize
        let collateral_units_underlying = net_value_usd
            .checked_mul(col_decimals_scale).ok_or(ErrorCode::MathError)?
            .checked_div(col_price).ok_or(ErrorCode::MathError)? as u64;

        // Convert to shares via ER
        let er_col = supply_exchange_rate(col_m)?;
        let collateral_shares_to_seize = ((collateral_units_underlying as u128)
            .checked_mul(SCALE_1E12).ok_or(ErrorCode::MathError)?)
            .checked_div(er_col).ok_or(ErrorCode::MathError)? as u64;

        require!(u_col.supply_shares >= collateral_shares_to_seize, ErrorCode::InsufficientBalance);

        // Update debt state
        let new_debt = current_debt
            .checked_sub(repay_capped as u128).ok_or(ErrorCode::MathError)? as u64;
        u_debt.borrowed_principal = new_debt;
        u_debt.last_borrow_index = debt_m.borrow_index;
        debt_m.total_borrows = debt_m.total_borrows.checked_sub(repay_capped).ok_or(ErrorCode::MathError)?;

        // Burn seized collateral shares from user
        u_col.supply_shares = u_col.supply_shares.checked_sub(collateral_shares_to_seize).ok_or(ErrorCode::MathError)?;
        col_m.total_supply_shares = col_m.total_supply_shares.checked_sub(collateral_shares_to_seize).ok_or(ErrorCode::MathError)?;

        // Transfer repay tokens from liquidator to pool vault
        token::transfer(
            CpiContext::new(
                ctx.accounts.token_program.to_account_info(),
                token::Transfer {
                    from: ctx.accounts.liquidator_debt_ata.to_account_info(),
                    to: ctx.accounts.debt_supply_vault.to_account_info(),
                    authority: ctx.accounts.liquidator.to_account_info(),
                },
            ),
            repay_capped,
        )?;

        // Transfer seized collateral underlying from collateral supply_vault to liquidator
        let seeds = &[b"market", col_m.controller.as_ref(), col_m.mint.as_ref(), &[col_m.bump]];
        let signer = &[&seeds[..]];
        let underlying_collateral = ((collateral_shares_to_seize as u128)
            .checked_mul(er_col).ok_or(ErrorCode::MathError)?)
            .checked_div(SCALE_1E12).ok_or(ErrorCode::MathError)? as u64;

        token::transfer(
            CpiContext::new_with_signer(
                ctx.accounts.token_program.to_account_info(),
                token::Transfer {
                    from: ctx.accounts.collateral_supply_vault.to_account_info(),
                    to: ctx.accounts.liquidator_collateral_ata.to_account_info(),
                    authority: col_m.to_account_info(),
                },
                signer,
            ),
            underlying_collateral,
        )?;

        // Protocol fee: take from collateral pool into reserves by reducing assets claimable
        // We deduct protocol_fee in underlying units from the pool by increasing total_reserves
        let protocol_fee_underlying = protocol_fee_usd
            .checked_mul(col_decimals_scale).ok_or(ErrorCode::MathError)?
            .checked_div(col_price).ok_or(ErrorCode::MathError)? as u64;
        

        // Transfer protocol fee from collateral supply vault to protocol treasury ATA
        let seeds = &[b"market", col_m.controller.as_ref(), col_m.mint.as_ref(), &[col_m.bump]];
        let signer = &[&seeds[..]];
        token::transfer(
            CpiContext::new_with_signer(
                ctx.accounts.token_program.to_account_info(),
                token::Transfer {
                from: ctx.accounts.collateral_supply_vault.to_account_info(),
                to: ctx.accounts.protocol_treasury_ata.to_account_info(),
                authority: col_m.to_account_info(),
                },
                signer,
            ),
            protocol_fee_underlying,
        )?;

        emit!(LiquidateEvent {
            liquidator: ctx.accounts.liquidator.key(),
            user: u_debt.user,
            debt_market: debt_m.key(),
            collateral_market: col_m.key(),
            repay_amount: repay_capped,
            collateral_seized_shares: collateral_shares_to_seize,
            collateral_underlying: underlying_collateral,
            protocol_fee_underlying: protocol_fee_underlying,
        });

        Ok(())
    }

    pub fn update_market(ctx: Context<UpdateMarket>) -> Result<()> {
        let m = &mut ctx.accounts.market;
        update_interest(m)?;
        emit!(RateUpdateEvent {
            market: m.key(),
            supply_rate_bps: m.supply_rate_bps,
            borrow_rate_bps: m.borrow_rate_bps,
            utilization_bps: m.utilization_bps,
        });
        Ok(())
    }
}

// Contexts

#[derive(Accounts)]
pub struct InitializeController<'info> {
    #[account(mut)]
    pub authority: Signer<'info>,
    /// CHECK: treasury can be PDA or EOA
    pub treasury: UncheckedAccount<'info>,
    #[account(
        init,
        payer = authority,
        space = 8 + 32 + 32 + 8*3 + 1 + 1 + 8,
        seeds = [b"controller", authority.key().as_ref()],
        bump
    )]
    pub controller: Account<'info, Controller>,
    pub system_program: Program<'info, System>,
}

#[derive(Accounts)]
pub struct CreateMarket<'info> {
    #[account(mut)]
    pub authority: Signer<'info>,
    #[account(mut, has_one = authority)]
    pub controller: Account<'info, Controller>,
    pub mint: Account<'info, Mint>,
    #[account(
        init,
        payer = authority,
        space = 8 + 32*3 + 1 + 8*17 + 16*2 + 1,
        seeds = [b"market", controller.key().as_ref(), mint.key().as_ref()],
        bump
    )]
    pub market: Account<'info, Market>,
    #[account(
        init,
        payer = authority,
        token::mint = mint,
        token::authority = market
    )]
    pub supply_vault: Account<'info, TokenAccount>,
    pub token_program: Program<'info, Token>,
    pub system_program: Program<'info, System>,
}

#[derive(Accounts)]
pub struct AdminUpdateMarket<'info> {
    pub authority: Signer<'info>,
    #[account(mut, has_one = authority)]
    pub controller: Account<'info, Controller>,
    #[account(mut, has_one = controller)]
    pub market: Account<'info, Market>,
}

#[derive(Accounts)]
pub struct SetPaused<'info> {
    pub authority: Signer<'info>,
    #[account(mut, has_one = authority)]
    pub controller: Account<'info, Controller>,
}

#[derive(Accounts)]
pub struct Supply<'info> {
    #[account(mut)]
    pub user: Signer<'info>,
    #[account(has_one = authority)]
    pub controller: Account<'info, Controller>,
    /// CHECK:
    pub authority: UncheckedAccount<'info>,
    #[account(mut, has_one = controller)]
    pub market: Account<'info, Market>,
    #[account(
        init_if_needed,
        payer = user,
        space = 8 + 32 + 32 + 8*2 + 16*2 + 1,
        seeds = [b"position", user.key().as_ref(), market.key().as_ref()],
        bump
    )]
    pub position: Account<'info, UserPosition>,
    #[account(
        mut,
        constraint = user_ata.mint == market.mint @ ErrorCode::InvalidMint,
        constraint = user_ata.owner == user.key() @ ErrorCode::InvalidOwner
    )]
    pub user_ata: Account<'info, TokenAccount>,
    #[account(
        mut,
        address = market.supply_vault @ ErrorCode::InvalidVault,
        constraint = supply_vault.mint == market.mint @ ErrorCode::InvalidMint,
        constraint = supply_vault.owner == market.key() @ ErrorCode::InvalidAuthority
    )]
    pub supply_vault: Account<'info, TokenAccount>,
    pub token_program: Program<'info, Token>,
    pub system_program: Program<'info, System>,
}

#[derive(Accounts)]
pub struct Redeem<'info> {
    #[account(mut)]
    pub user: Signer<'info>,
    pub controller: Account<'info, Controller>,
    #[account(mut, has_one = controller)]
    pub market: Account<'info, Market>,
    #[account(
        mut,
        seeds = [b"position", user.key().as_ref(), market.key().as_ref()],
        bump
    )]
    pub position: Account<'info, UserPosition>,
    #[account(
        mut,
        address = market.supply_vault @ ErrorCode::InvalidVault,
        constraint = supply_vault.mint == market.mint @ ErrorCode::InvalidMint,
        constraint = supply_vault.owner == market.key() @ ErrorCode::InvalidAuthority
    )]
    pub supply_vault: Account<'info, TokenAccount>,
    #[account(
        mut,
        constraint = user_ata.mint == market.mint @ ErrorCode::InvalidMint,
        constraint = user_ata.owner == user.key() @ ErrorCode::InvalidOwner
    )]
    pub user_ata: Account<'info, TokenAccount>,
    pub token_program: Program<'info, Token>,
    // remaining_accounts: validated positions set per registry for portfolio checks
}

#[derive(Accounts)]
pub struct SetCollateral<'info> {
    pub user: Signer<'info>,
    #[account(has_one = controller)]
    pub market: Account<'info, Market>,
    pub controller: Account<'info, Controller>,
    #[account(
        mut,
        seeds = [b"position", user.key().as_ref(), market.key().as_ref()],
        bump
    )]
    pub position: Account<'info, UserPosition>,
    // remaining_accounts for portfolio checks if disabling
}

#[derive(Accounts)]
pub struct Borrow<'info> {
    #[account(mut)]
    pub user: Signer<'info>,
    pub controller: Account<'info, Controller>,
    #[account(mut, has_one = controller)]
    pub borrow_market: Account<'info, Market>,
    #[account(
        mut,
        seeds = [b"position", user.key().as_ref(), borrow_market.key().as_ref()],
        bump
    )]
    pub borrower_position: Account<'info, UserPosition>,
    #[account(
        mut,
        constraint = user_borrow_ata.mint == borrow_market.mint @ ErrorCode::InvalidMint,
        constraint = user_borrow_ata.owner == user.key() @ ErrorCode::InvalidOwner
    )]
    pub user_borrow_ata: Account<'info, TokenAccount>,
    #[account(
        mut,
        address = borrow_market.supply_vault @ ErrorCode::InvalidVault,
        constraint = supply_vault.mint == borrow_market.mint @ ErrorCode::InvalidMint,
        constraint = supply_vault.owner == borrow_market.key() @ ErrorCode::InvalidAuthority
    )]
    pub supply_vault: Account<'info, TokenAccount>,
    pub token_program: Program<'info, Token>,
    // remaining_accounts: validated positions per registry
}

#[derive(Accounts)]
pub struct Repay<'info> {
    #[account(mut)]
    pub user: Signer<'info>,
    pub controller: Account<'info, Controller>,
    #[account(mut, has_one = controller)]
    pub borrow_market: Account<'info, Market>,
    #[account(
        mut,
        seeds = [b"position", user.key().as_ref(), borrow_market.key().as_ref()],
        bump
    )]
    pub borrower_position: Account<'info, UserPosition>,
    #[account(
        mut,
        constraint = user_repay_ata.mint == borrow_market.mint @ ErrorCode::InvalidMint,
        constraint = user_repay_ata.owner == user.key() @ ErrorCode::InvalidOwner
    )]
    pub user_repay_ata: Account<'info, TokenAccount>,
    #[account(
        mut,
        address = borrow_market.supply_vault @ ErrorCode::InvalidVault,
        constraint = supply_vault.mint == borrow_market.mint @ ErrorCode::InvalidMint,
        constraint = supply_vault.owner == borrow_market.key() @ ErrorCode::InvalidAuthority
    )]
    pub supply_vault: Account<'info, TokenAccount>,
    pub token_program: Program<'info, Token>,
}

#[derive(Accounts)]
pub struct Liquidate<'info> {
    #[account(mut)]
    pub liquidator: Signer<'info>,
    pub controller: Account<'info, Controller>,

    // Debt side
    #[account(mut, has_one = controller)]
    pub debt_market: Account<'info, Market>,
    #[account(
        mut,
        // PDA seeds still ensure same user in both positions
        seeds = [b"position", user_debt_position.user.as_ref(), debt_market.key().as_ref()],
        bump
    )]
    pub user_debt_position: Account<'info, UserPosition>,
    #[account(
        mut,
        address = debt_market.supply_vault @ ErrorCode::InvalidVault,
        constraint = debt_supply_vault.mint == debt_market.mint @ ErrorCode::InvalidMint,
        constraint = debt_supply_vault.owner == debt_market.key() @ ErrorCode::InvalidAuthority
    )]
    pub debt_supply_vault: Account<'info, TokenAccount>,
    #[account(
        mut,
        constraint = liquidator_debt_ata.mint == debt_market.mint @ ErrorCode::InvalidMint,
        constraint = liquidator_debt_ata.owner == liquidator.key() @ ErrorCode::InvalidOwner
    )]
    pub liquidator_debt_ata: Account<'info, TokenAccount>,

    // Collateral side
    #[account(mut, has_one = controller)]
    pub collateral_market: Account<'info, Market>,
    #[account(
        mut,
        seeds = [b"position", user_debt_position.user.as_ref(), collateral_market.key().as_ref()],
        bump
    )]
    pub user_collateral_position: Account<'info, UserPosition>,
    #[account(
        mut,
        address = collateral_market.supply_vault @ ErrorCode::InvalidVault,
        constraint = collateral_supply_vault.mint == collateral_market.mint @ ErrorCode::InvalidMint,
        constraint = collateral_supply_vault.owner == collateral_market.key() @ ErrorCode::InvalidAuthority
    )]
    pub collateral_supply_vault: Account<'info, TokenAccount>,
    #[account(
        mut,
        constraint = liquidator_collateral_ata.mint == collateral_market.mint @ ErrorCode::InvalidMint,
        constraint = liquidator_collateral_ata.owner == liquidator.key() @ ErrorCode::InvalidOwner
    )]
    pub liquidator_collateral_ata: Account<'info, TokenAccount>,

    #[account(
        mut,
        constraint = protocol_treasury_ata.mint == collateral_market.mint @ ErrorCode::InvalidMint,
        constraint = protocol_treasury_ata.owner == controller.treasury @ ErrorCode::InvalidOwner
    )]
    pub protocol_treasury_ata: Account<'info, TokenAccount>,

    /// CHECK: replace with actual oracle accounts and validations
    pub debt_oracle: UncheckedAccount<'info>,
    /// CHECK:
    pub collateral_oracle: UncheckedAccount<'info>,

    pub token_program: Program<'info, Token>,

    // remaining_accounts: other positions/oracles for full portfolio
    // plus a UserRegistry account passed separately:
    #[account(
        constraint = user_registry.user == user_debt_position.user @ ErrorCode::RegistryMismatch,
        constraint = user_registry.controller == controller.key() @ ErrorCode::RegistryMismatch
    )]
    pub user_registry: Account<'info, UserRegistry>,
}

#[derive(Accounts)]
pub struct UpdateMarket<'info> {
    #[account(mut)]
    pub market: Account<'info, Market>,
}

// Helpers
fn update_interest(m: &mut Market) -> Result<()> {
    let now_slot = Clock::get()?.slot;
    let slots = now_slot.saturating_sub(m.last_update_slot);
    if slots == 0 {
        return Ok(());
    }

    // Compute utilization = borrows / (cash + borrows - reserves)
    // cash approximated by vault balance should be read for precision; for deterministic math,
    // we can approximate total_assets via shares*ER. We'll use total_assets() function.
    let total_assets = total_assets_underlying(m)?;
    let borrows = m.total_borrows as u128;
    let reserves = m.total_reserves as u128;

    let denom = total_assets.saturating_sub(reserves);
    let util_bps = if denom == 0 {
        0
    } else {
        borrows
            .checked_mul(BASIS_POINTS as u128).ok_or(ErrorCode::MathError)?
            .checked_div(denom).ok_or(ErrorCode::MathError)? as u64
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

    // Per-slot scaled increments (approximate linear accrual over slots)
    let slots_per_year = (SECONDS_PER_YEAR as u128).checked_mul(2).ok_or(ErrorCode::MathError)?;
    let r_borrow_scaled_per_slot = (borrow_bps as u128)
        .checked_mul(SCALE_1E12).ok_or(ErrorCode::MathError)?
        .checked_div(slots_per_year).ok_or(ErrorCode::MathError)?
        .checked_div(BASIS_POINTS as u128).ok_or(ErrorCode::MathError)?;
    let r_supply_scaled_per_slot = (supply_bps as u128)
        .checked_mul(SCALE_1E12).ok_or(ErrorCode::MathError)?
        .checked_div(slots_per_year).ok_or(ErrorCode::MathError)?
        .checked_div(BASIS_POINTS as u128).ok_or(ErrorCode::MathError)?;

    // Apply once for all slots: index += index * (r_per_slot * slots / 1e12)
    let borrow_incr_scaled = r_borrow_scaled_per_slot
        .checked_mul(slots as u128).ok_or(ErrorCode::MathError)?;
    let supply_incr_scaled = r_supply_scaled_per_slot
        .checked_mul(slots as u128).ok_or(ErrorCode::MathError)?;

    m.borrow_index = m.borrow_index.checked_add(
        m.borrow_index
            .checked_mul(borrow_incr_scaled).ok_or(ErrorCode::MathError)?
            .checked_div(SCALE_1E12).ok_or(ErrorCode::MathError)?,
    ).ok_or(ErrorCode::MathError)?;

    m.supply_index = m.supply_index.checked_add(
        m.supply_index
            .checked_mul(supply_incr_scaled).ok_or(ErrorCode::MathError)?
            .checked_div(SCALE_1E12).ok_or(ErrorCode::MathError)?,
    ).ok_or(ErrorCode::MathError)?;

    // Reserves accrual: interest accrued to borrows over slots times reserve factor
    // Approximate interest on borrows = total_borrows * (borrow_incr_scaled / 1e12)
    let interest_on_borrows = (m.total_borrows as u128)
        .checked_mul(borrow_incr_scaled).ok_or(ErrorCode::MathError)?
        .checked_div(SCALE_1E12).ok_or(ErrorCode::MathError)? as u64;
    let to_reserves = (interest_on_borrows as u128)
        .checked_mul(m.reserve_factor_bps as u128).ok_or(ErrorCode::MathError)?
        .checked_div(BASIS_POINTS as u128).ok_or(ErrorCode::MathError)? as u64;
    if to_reserves > 0 {
        m.total_reserves = m.total_reserves.checked_add(to_reserves).ok_or(ErrorCode::MathError)?;
    }

    m.last_update_slot = now_slot;
    Ok(())
}

// Exchange rate underlying per share; with reserves, ER grows as assets grow
fn supply_exchange_rate(m: &Market) -> Result<u128> {
    if m.total_supply_shares == 0 {
        return Ok(SCALE_1E12);
    }
    // total_assets = cash + borrows - reserves; modeled via shares * ER
    // We approximate with supply_index as growth proxy; you can compute ER from assets/shares.
    // For internal consistency, keep using supply_index as ER.
    Ok(m.supply_index)
}

fn total_assets_underlying(m: &Market) -> Result<u128> {
    if m.total_supply_shares == 0 {
        return Ok(0);
    }
    let er = supply_exchange_rate(m)?;
    let tot = ((m.total_supply_shares as u128)
        .checked_mul(er).ok_or(ErrorCode::MathError)?)
        .checked_div(SCALE_1E12).ok_or(ErrorCode::MathError)?;
    Ok(tot)
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

// Placeholder: enforce oracle matches and return price in quote per whole token as u128
fn get_price_checked(
    controller: &Account<Controller>,
    market: &Market,
    oracle_ai: &AccountInfo,
) -> Result<u128> {
    let _ = controller;
    require_keys_eq!(oracle_ai.key(), market.oracle, ErrorCode::InvalidOracle);
    // TODO: real oracle read with staleness & conf checks; return e.g. USD price with 1e8 scale
    // For this scaffold, return 1_0000_0000 (1 USD) scaled 1e8
    Ok(1_0000_0000u128)
}

// Registry validation: ensure remaining triplets cover all registry markets
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
    // ensure registry.markets (first count entries) are all present
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
    use_ltv: bool,                               // if true, use collateral_factor; else liquidation_threshold
) -> Result<(u128, u128, Pubkey)> {
    require!(remaining.len() % 3 == 0, ErrorCode::AccountMismatch);
    if remaining.is_empty() {
        return Ok((0, 0, controller.authority));
    }

    let mut total_debt_value: u128 = 0;
    let mut total_collateral_value_limit: u128 = 0; // either LTV-based or threshold-based
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

        require_keys_eq!(market.controller, controller.key(), ErrorCode::AccountMismatch);
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

        // Collateral contribution
        if position.collateral_enabled && position.supply_shares > 0 {
            let er = supply_exchange_rate(&market)?;
            let underlying = (position.supply_shares as u128)
                .checked_mul(er).ok_or(ErrorCode::MathError)?
                .checked_div(SCALE_1E12).ok_or(ErrorCode::MathError)? as u64;

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

        // Debt contribution
        let accrued = accrue_debt(&position, &market)?;
        if accrued > 0 {
            let debt_usd = value_usd_from_underlying(accrued as u64, price_usd, market.mint_decimals)?;
            total_debt_value = total_debt_value.checked_add(debt_usd).ok_or(ErrorCode::MathError)?;
        }

        // Hypothetical redeem effect
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

        // Hypothetical new borrow effect
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

// LTV checks
fn enforce_borrow_power_after_hypo_redeem<'info>(
    controller: &Account<'info, Controller>,
    remaining: &'info [AccountInfo<'info>],
    hypo_redeem: (Pubkey, u64, u64), // (market, shares, underlying)
    decimals: u8,
) -> Result<()> {
    let (total_debt, total_borrow_power, _user) = aggregate_portfolio_from_remaining(
        controller,
        remaining,
        Some((hypo_redeem.0, hypo_redeem.1, hypo_redeem.2, decimals)),
        None,
        true, // use LTV
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
        false, // use liquidation threshold
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