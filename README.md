# Trustless Airdrop Smart Contracts  

This project enables large-scale, high-throughput trustless airdrops on the Cardano blockchain with no UTxO contention. It supports participants from multiple blockchain networks, allowing them to claim their airdrop by providing a signed message proving ownership of their public keys on their respective chains.

## Supported Blockchain Networks

- Cardano
- Ethereum
- Solana
- Bitcoin

## Features

- Cross-chain compatibility
- High throughput
- No UTxO contention
- Trustless verification

## How It Works

1. Create a Merkle Patricia Forestry with user public keys and airdrop amounts.
2. Parameterize and deploy scripts as reference scripts.
3. Initialize the on-chain linked list.
4. Users can claim their airdrop by providing a signed message from their respective blockchain that proves ownership of a public key along with a proof that their public key has an aidrop allocation in the Merkle Patricia Forestry.

### Creating a Merkle Patricia Forestry

To launch an airdrop, first create a Merkle Patricia Forestry with the public keys of the users and the amount of tokens they will receive. Here's an example using the `@aiken-lang/merkle-patricia-forestry` library:

```javascript
import { Store, Trie } from '@aiken-lang/merkle-patricia-forestry';
const rawData = [
    { key: '0x049b3472ebe63bff0f092d7e3c464169829c4369', value: '168285894526485760' },
    { key: '0x4b4dc012894fa59917b1155f348e639059ed6238', value: '204169096277495776' },
    // ... more key-value pairs ...
    ];
console.log("Building Merkle Patricia Trie...");
const data = rawData.map(item => ({
    key: Buffer.from(item.key.slice(2), 'hex'),
    value: item.value
    }));
const trie = await Trie.fromList(data);
```

### Deploying and Initializing the Airdrop

1. Parameterize the scripts and deploy them as reference scripts.
- Use a Cardano CLI or a library like `lucid-evolution` to submit the transactions that deploy these scripts.

2. Initialize the on-chain linked list by consuming the `initUTxO` ref while invoking `mkAirdropNodeMP` with the `PLInit` redeemer.

```javascript
export const initAirdropNode = async (
  lucid: Lucid,
  config: InitNodeConfig,
): Promise<Result<TxComplete>> => {
  const nodeValidator: SpendingValidator = {
    type: "PlutusV2",
    script: config.scripts.nodeValidator,
  };

  const nodeValidatorAddr = lucid.utils.validatorToAddress(nodeValidator);

  const nodePolicy: MintingPolicy = {
    type: "PlutusV2",
    script: config.scripts.nodePolicy,
  };

  const nodePolicyId = lucid.utils.mintingPolicyToId(nodePolicy);

  const assets = {
    [toUnit(nodePolicyId, originNodeTokenName)]: 1n,
    // [toUnit(nodePolicyId, corrNodeTokenName)]: 1n,
  };

  const datum = Data.to(
    {
      key: null,
      next: null,
      commitment: BigInt(0),
    },
    AirdropSetNode,
  );

  const liquidityNodePolicyRedeemer = Data.to("PLInit", AirdropNodeAction);

  try {
    const tx = await lucid
      .newTx()
      .collectFrom([config.initUTXO])
      .payToContract(
        nodeValidatorAddr,
        { inline: datum },
        { ...assets, lovelace: TT_UTXO_ADDITIONAL_ADA + MIN_COMMITMENT_ADA },
      )
      .mintAssets(assets, airdropNodePolicyRedeemer)
      .compose(
        config.refScripts?.nodePolicy
          ? lucid.newTx().readFrom([config.refScripts.nodePolicy])
          : lucid.newTx().attachMintingPolicy(nodePolicy),
      )
      .complete();

    return { type: "ok", data: tx };
  } catch (error) {
    if (error instanceof Error) return { type: "error", error: error };

    return { type: "error", error: new Error(`${JSON.stringify(error)}`) };
  }
};
```

### Claiming an Airdrop

Users can claim their airdrop by following these steps:

1. Generate a signed message proving ownership of their public key on their respective blockchain.
2. Obtain a Merkle proof of their allocation in the Merkle Patricia Forestry.
3. Submit a transaction to the Cardano blockchain that includes:
   - Pubic Key
   - Network (ie. Cardano / Ethereum / Solana ...)
   - The signed message
   - The Merkle Patricia Forestry membership proof 

The membership proof can be computed off-chain using the `@aiken-lang/merkle-patricia-forestry` library:
```javascript
const proofPublicKey = await trie.prove(Buffer.from('0x3c8e73a95798dbeb720709820a66f77ee13502a9'.slice(2), 'hex'));
console.log(proofPublicKey.toCBOR())
```



# Set up nix config 
Put the following lines in your nix configuration file (usually located at /etc/nix/nix.conf)

extra-experimental-features = nix-command flakes ca-derivations
extra-trusted-substituters = https://cache.iog.io https://cache.nixos.org/ https://public-plutonomicon.cachix.org https://cache.zw3rk
extra-trusted-public-keys = cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=

# Installation 
After setting up nix config, restart your computer or VM. 
Then run:
    nix develop 

# License
See the [LICENSE](LICENSE) file for license rights and limitations (MIT).