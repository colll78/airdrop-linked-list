{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module LiquidityEvent.Validator (
  pAirdropSetValidator,
  pLiquidityGlobalLogicW
) where

import Data.ByteString (ByteString)

import Plutarch (Config)
import Plutarch.LedgerApi.V1 (
  PCredential (..),
 )
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Value (plovelaceValueOf, pnoAdaValue, pvalueOf)
import Plutarch.LedgerApi.Value qualified as Value 
import Plutarch.LedgerApi.V2 hiding (PScriptContext)
import Plutarch.LedgerApi.V3 
import Plutarch.LedgerApi.Interval (pafter, pbefore)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PriceDiscoveryEvent.Utils (pvalidityRangeStart, pletFieldsSpending, pfromPDatum, passert, pcontainsCurrencySymbols, pfindCurrencySymbolsByTokenPrefix, ptryOwnInput, ptryOwnOutput, phasCS, pisFinite, pmustFind, pand'List)
import Types.Constants (rewardFoldTN)
import Types.LiquiditySet (PNodeKey(..), PClaimValidatorConfig (..), PAirdropSetNode (..), PLNodeAction (..))
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

pAirdropSetValidator ::
  Config ->
  ByteString ->
  ClosedTerm (PClaimValidatorConfig :--> PScriptContext :--> PUnit)
pAirdropSetValidator cfg prefix = plam $ \claimConfig ctx' -> P.do 
  ctx <- pletFields @'["txInfo", "scriptInfo", "redeemer"] ctx'
  let redeemer = punsafeCoerce @_ @_ @PLNodeAction (pto ctx.redeemer)
  scriptInfoF <- pletFieldsSpending (pforgetData ctx.scriptInfo)
  ownInputRef <- plet scriptInfoF._0

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
      passert
        "Must mint/burn for any linked list interaction"
        (pcontainsCurrencySymbols # pfromData info.mint # potentialNodeCSs)
      (pconstant ())
    PPartialUnlock _ -> P.do
      claimConfigF <- pletFields @'["vestingPeriodStart", "vestingPeriodEnd"]
      PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatch (pfield @"credential" # ownInputF.address)
      let ownOutput = ptryOwnOutput # info.outputs # ownValHash
      ownOutputF <- pletFields @'["value", "datum"] ownOutput
      POutputDatum ((pfield @"outputDatum" #) -> ownOutputDatum) <- pmatch ownOutputF.datum
      
      PPubKeyCredential ((pfield @"_0" #) -> beneficiaryHash) <- pmatch (pfield @"credential" # vestingDatumF.beneficiary)
      PDJust ((pfield @"_0" #) -> ownInDat) <- pmatch scriptInfoF._1
      ownInputDatumF <- pletFields @'["extraData"] (punsafeCoerce @_ @_ @PAirdropSetNode (pto ownInDat))
      vestingDatumF <- pletFields @'["beneficiary", "totalVestingQty"] (punsafeCoerce @_ @_ @PVestingDatum ownInputDatumF.extraData)
      let currentTimeApproximation = pvalidityRangeStart # info.validRange 
          vestingTimeRemaining = (pfromData claimConfigF.vestingPeriodEnd) - currentTimeApproximation
          vestingPeriodLength = (pfromData claimConfigF.vestingPeriodEnd) - (pfromData claimConfigF.vestingPeriodStart)
          timeBetweenInstallments = pdivCeil # vestingPeriodLength # totalVestingInstallments
          futureInstallments = pdivCeil # vestingTimeRemaining # timeBetweenInstallments
          expectedRemainingQty = pdivCeil # (futureInstallments * vestingDatumF.totalVestingQty) #$ totalVestingInstallments

      ownInputVal <- plet ownInputF.value
      inputVestingTokens <- pvalueOf # claimTokenCS # claimTokenTN # ownInputVal
      let vestingTokenDelta = expectedRemainingQty - inputVestingTokens 
          claimedTokens = Value.psingleton # claimTokenCS # claimTokenTN # vestingTokenDelta
      let checks = 
            pand'List
             [ (pto ownInDat) #== ownOutputDatum
             , pelem # beneficiaryHash # info.signatories
             , pfromData info.mint #== mempty
             , (ownInputVal <> claimedTokens) #== pforgetPositive outputNodeF.value
             ]
      pif checks 
          (pconstant ()) 
          perror 
  
-- TODO PlaceHolder # This contribution holds only the minimum amount of Ada + the FoldingFee, it cannot be updated. It cannot be removed until the reward fold has completed.