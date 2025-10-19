use anchor_lang::prelude::*;

declare_id!("5VpNKsjLNdkvh8x1tdcR2Y1Fft9457SVfsvtkvTLqwGb");

#[program]
pub mod anchor_obscure {
    use super::*;

    pub fn initialize_protocol(ctx: Context<InitializeProtocol>, protocol_fee: u64) -> Result<()> {
        let protocol_state = &mut ctx.accounts.protocol_state;
        protocol_state.authority = ctx.accounts.authority.key();
        protocol_state.protocol_fee = protocol_fee;
        protocol_state.total_markets = 0;
        Ok(())
    }
}

// Context Structures
#[derive(Accounts)]
pub struct InitializeProtocol<'info> {
    #[account(mut)]
    pub authority: Signer<'info>,
    
    #[account(
        init,
        payer = authority,
        space = 8 + 32 + 8 + 8,
        seeds = [b"protocol_state"],
        bump,
    )]
    pub protocol_state: Account<'info, ProtocolState>,
    pub system_program: Program<'info, System>,
}
#[account]
pub struct ProtocolState {
    pub authority: Pubkey,
    pub protocol_fee: u64,
    pub total_markets: u64,
}

#[account]
pub struct Market {
    pub mint: Pubkey,
    pub supply_vault: Pubkey,
    pub borrow_vault: Pubkey,
    
    // Interest rate model
    pub base_rate: u64,
    pub slope1: u64,
    pub slope2: u64,
    pub kink_utilization: u64,
    pub reserve_factor: u64,
    
    // Risk parameters
    pub liquidation_threshold: u64,
    pub ltv_ratio: u64,
    
    // Market state
    pub total_supply: u64,
    pub total_borrows: u64,
    pub total_reserves: u64,
    pub supply_index: u128,
    pub borrow_index: u128,
    pub supply_rate: u64,
    pub borrow_rate: u64,
    pub utilization_rate: u64,
    pub last_update_slot: u64,
    pub bump: u8,
}

#[account]
pub struct UserPosition {
    pub user: Pubkey,
    pub market: Pubkey,
    pub supplied_amount: u64,  // cToken amount
    pub borrowed_amount: u64,  // Underlying token amount
    pub supply_index: u128,
    pub borrow_index: u128,
    pub last_reward_claim_slot: u64,
}