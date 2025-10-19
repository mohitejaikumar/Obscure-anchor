import * as anchor from "@coral-xyz/anchor";
import { Program } from "@coral-xyz/anchor";
import { AnchorObscure } from "../target/types/anchor_obscure";
import { Keypair, LAMPORTS_PER_SOL, PublicKey, SystemProgram } from "@solana/web3.js";
import { assert } from "chai";

describe("anchor-obscure", () => {
  // Configure the client to use the local cluster.
  anchor.setProvider(anchor.AnchorProvider.env());

  const program = anchor.workspace.anchorObscure as Program<AnchorObscure>;
  const provider = anchor.getProvider() as anchor.AnchorProvider;

  let authority: Keypair;
  let protocolState: PublicKey;

  before(async ()=> {
    authority = Keypair.generate();

    await Promise.all([
      provider.connection.requestAirdrop(authority.publicKey, LAMPORTS_PER_SOL * 1000),
    ])
    console.log("Successfully airdropped 1000 SOL to authority");

    await new Promise(resolve => setTimeout(resolve, 1000));

    [protocolState, ] = PublicKey.findProgramAddressSync([Buffer.from("protocol_state")], program.programId);
  })


  it("Is initialized!", async () => {
    
    const tx = await program.methods.initializeProtocol(new anchor.BN(100)).accountsStrict({
      authority: authority.publicKey,
      protocolState: protocolState,
      systemProgram: SystemProgram.programId,
    })
    .signers([authority])
    .rpc();

    console.log("Successfully initialized protocol", tx);

    const protocolStateAccount = await program.account.protocolState.fetch(protocolState);
    console.log("Protocol state:", protocolStateAccount);

    assert.equal(protocolStateAccount.authority.toString(), authority.publicKey.toString());
    assert.equal(protocolStateAccount.protocolFee.toString(), new anchor.BN(100).toString());
    assert.equal(protocolStateAccount.totalMarkets.toString(), new anchor.BN(0).toString());
  });
});
