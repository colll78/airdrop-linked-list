{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module AirdropEvent.Mint.AirdropToken (
  mkAirdropTokenMP,
) where

import Plutarch.LedgerApi.V3
import Plutarch.LedgerApi.Interval (pafter, pbefore)

import Plutarch.Internal (Config (..))
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Prelude
import Airdrop.Utils (pand'List, passert, pcond, pisFinite, phasUTxO, pintToByteString)
import Types.AirdropSet (PAirdropConfig (..), PAirdropMintAction (..))
import Types.Constants (claimRoot, claimTokenCS, ptryParseNodeKey)
import Plutarch.Builtin (pserialiseData, pforgetData, PDataNewtype(..))
import MerkleTree.MerklePatriciaForestry (phas)
import Scripts.NodeMP (airdropNodeCS)
import Airdrop.Utils (pcond, pand'List, pmustFind)

--------------------------------
-- TOken to Airdrop Minting Policy:
--------------------------------

mkAirdropTokenMP :: ClosedTerm ( PScriptContext :--> PUnit )
mkAirdropTokenMP = plam $ \ctx -> P.do
  ctxF <- pletFields @'["txInfo", "redeemer", "scriptInfo"] ctx 
  infoF <- pletFields @'["outputs", "mint"]
  let red = punsafeCoerce @_ @_ @PAirdropMintAction (pto (pfield @"redeemer" # ctx))
  PMintingScript scriptInfo <- pmatch ctxF.scriptInfo 
  ownCS <- plet $ pfield @"_0" # scriptInfo 
  pmatch redm $ \case
    PMintAirdrop _ -> P.do
      let tkPairs = ptryLookupValue # pconstant airdropNodeCS # infoF.mint 
      tkPair <- plet (pheadSingleton # tkPairs)
      insertedName <- plet (pfstBuiltin # tkPair)
      let nodeOutput = pmustFind @PBuiltinList (\txo -> pvalueOf # (pfield @"value" # txo) # pconstant airdropNodeCS # insertedName == 1) infoF.outputs
      (pconstant ())
    PBurnAirdrop _ -> P.do
      perror

