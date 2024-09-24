{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module LiquidityEvent.Validator (
  pLiquiditySetValidator,
  pLiquidityGlobalLogicW
) where

import Data.ByteString (ByteString)

import Plutarch (Config)
import Plutarch.LedgerApi.V1 (
  PCredential (..),
 )
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Value (plovelaceValueOf, pnoAdaValue, pvalueOf)
import Plutarch.LedgerApi.V2 hiding (PScriptContext)
import Plutarch.LedgerApi.V3 
import Plutarch.LedgerApi.Interval (pafter, pbefore)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PriceDiscoveryEvent.Utils (pletFieldsSpending, pfromPDatum, passert, pcontainsCurrencySymbols, pfindCurrencySymbolsByTokenPrefix, ptryOwnInput, ptryOwnOutput, phasCS, pisFinite, pmustFind, pand'List)
import Types.Constants (rewardFoldTN)
import Types.LiquiditySet (PLBELockConfig (..), PLiquiditySetNode (..), PLNodeAction (..))
import Types.DiscoverySet (PNodeKey(..))
import Plutarch.Builtin (pforgetData)

pLiquidityGlobalLogicW :: Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
pLiquidityGlobalLogicW = phoistAcyclic $ plam $ \foldCS' ctx -> P.do
  -- let rewardsIdx = punsafeCoerce @_ @_ @(PAsData PInteger) redeemer
  ctxF <- pletFields @'["txInfo"] ctx
  infoF <- pletFields @'["inputs"] ctxF.txInfo
  foldCS <- plet $ pfromData foldCS'
  let hasFoldToken = pany @PBuiltinList # plam (\inp -> phasCS # (pfield @"value" # (pfield @"resolved" # inp)) # foldCS) # infoF.inputs 
  -- let rewardInp = pelemAt @PBuiltinList # pfromData rewardsIdx # infoF.inputs 
  --     hasFoldToken = pvalueOf # (pfield @"value" # (pfield @"resolved" # rewardInp)) # pfromData rewardCS # rewardFoldTN #== 1
  pif hasFoldToken (pconstant ()) perror 

pLiquiditySetValidator ::
  Config ->
  ByteString ->
  ClosedTerm (PLBELockConfig :--> PScriptContext :--> PUnit)
pLiquiditySetValidator cfg prefix = plam $ \discConfig ctx' -> P.do 
  ctx <- pletFields @'["txInfo", "scriptInfo", "redeemer"] ctx'
  let redeemer = punsafeCoerce @_ @_ @PLNodeAction (pto ctx.redeemer)
  scriptInfoF <- pletFieldsSpending (pforgetData ctx.scriptInfo)
  ownInputRef <- plet scriptInfoF._0
  PDJust ((pfield @"_0" #) -> ownInDat) <- pmatch scriptInfoF._1
  let oldDatum = punsafeCoerce @_ @_ @PLiquiditySetNode (pto ownInDat)

  info <- pletFields @'["inputs", "outputs", "mint", "validRange", "signatories", "referenceInputs"] ctx.txInfo
  txInputs <- plet info.inputs

  let ownInput = ptryOwnInput # txInputs # ownInputRef
        
  ownInputF <- pletFields @'["value", "address"] ownInput

  let ownInputValue = pfromData ownInputF.value

  pmatch redeemer $ \case  
    PLLinkedListAct _ -> P.do
      -- all those CSs has tokens that prefixed by Node prefix
      -- any of those can be actual Node CS
      let potentialNodeCSs = pfindCurrencySymbolsByTokenPrefix # ownInputValue # pconstant prefix
      -- TODO: Current launchpad token cannot start with FSN
      passert
        "Must mint/burn for any linked list interaction"
        (pcontainsCurrencySymbols # pfromData info.mint # potentialNodeCSs)
      (pconstant ())
    PPartialUnlock _ -> P.do
      -- PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatch (pfield @"credential" # ownInputF.address)
      -- configF <- pletFields @'["discoveryDeadline"] discConfig
      -- let ownOutput = ptryOwnOutput # info.outputs # ownValHash
      -- ownOutputF <- pletFields @'["value", "datum"] ownOutput
      -- POutputDatum ((pfield @"outputDatum" #) -> ownOutputDatum) <- pmatch ownOutputF.datum
      -- let newDatum = pfromPDatum @PLiquiditySetNode # ownOutputDatum
      -- passert "Cannot modify datum when committing" (newDatum #== oldDatum)
      -- passert "Cannot modify non-ada value" (pnoAdaValue # ownInputF.value #== pnoAdaValue # ownOutputF.value)
      -- passert "Cannot reduce ada value" (plovelaceValueOf # ownInputF.value #< plovelaceValueOf # ownOutputF.value - 10_000_000)
      -- passert "No tokens minted" (pfromData info.mint #== mempty)
      -- passert "deadline passed" ((pafter # (pfromData configF.discoveryDeadline - 86_400_000) # info.validRange))
      -- passert "vrange not finite" (pisFinite # info.validRange)
      (pconstant ()) 
    PFullUnlock _ -> P.do
      perror
  
-- TODO PlaceHolder # This contribution holds only the minimum amount of Ada + the FoldingFee, it cannot be updated. It cannot be removed until the reward fold has completed.