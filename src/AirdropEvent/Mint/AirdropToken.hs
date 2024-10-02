module AirdropEvent.Mint.AirdropToken (
  mkAirdropTokenMP,
) where

import Plutarch.LedgerApi.V3
import Plutarch.LedgerApi.Value (pvalueOf)
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Prelude
import Airdrop.Utils (pand'List, pheadSingleton, pmustFind, ptryLookupValue)
import Types.AirdropSet (PAirdropSetNode(..), PVestingDatum(..), PAirdropMintAction (..))
import Types.Constants (claimTokenTN)
import Scripts.NodeMP (airdropNodeCS)

--------------------------------
-- TOken to Airdrop Minting Policy:
--------------------------------

mkAirdropTokenMP :: ClosedTerm ( PScriptContext :--> PUnit )
mkAirdropTokenMP = plam $ \ctx -> P.do
  ctxF <- pletFields @'["txInfo", "redeemer", "scriptInfo"] ctx 
  infoF <- pletFields @'["outputs", "mint"] ctxF.txInfo
  let red = punsafeCoerce @_ @_ @PAirdropMintAction (pto (pfield @"redeemer" # ctx))
  PMintingScript scriptInfo <- pmatch ctxF.scriptInfo 
  ownCS <- plet $ pfield @"_0" # scriptInfo 
  mintedValue <- plet infoF.mint

  let ownTkPairs = ptryLookupValue # ownCS # mintedValue
  ownTkPair <- plet (pheadSingleton # ownTkPairs)
  ownTokenName <- plet (pfstBuiltin # ownTkPair)
  ownNumMinted <- plet (psndBuiltin # ownTkPair)

  pmatch red $ \case
    PMintAirdrop _ -> P.do
      let nodeTkPairs = ptryLookupValue # pconstantData airdropNodeCS # mintedValue 
      nodeTkPair <- plet (pheadSingleton # nodeTkPairs)
      insertedName <- plet $ pfstBuiltin # nodeTkPair
      let nodeOutput = pmustFind @PBuiltinList # plam (\txo -> pvalueOf # (pfield @"value" # txo) # pconstant airdropNodeCS # pfromData insertedName #== pconstant 1) # infoF.outputs
      nodeOutputF <- pletFields @'["value", "datum"] nodeOutput 
      ownInputDatumF <- pletFields @'["extraData"] (punsafeCoerce @_ @_ @PAirdropSetNode (pto $ nodeOutputF.datum))
      vestingDatumF <- pletFields @'["totalVestingQty"] (punsafeCoerce @_ @_ @PVestingDatum ownInputDatumF.extraData)

      let checks =
            pand'List 
             [ ownNumMinted #== vestingDatumF.totalVestingQty 
             , pvalueOf # nodeOutputF.value # pfromData ownCS # pfromData ownTokenName #== pfromData ownNumMinted
             , ownTokenName #== pdata claimTokenTN
             ]
      pif checks 
          (pconstant ())
          perror 
    PBurnAirdrop _ -> P.do
      perror

