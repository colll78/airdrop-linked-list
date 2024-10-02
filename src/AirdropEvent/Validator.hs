{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module AirdropEvent.Validator where 

import Data.ByteString (ByteString)

import Plutarch (Config)
import Plutarch.LedgerApi.V1 (
  PCredential (..),
 )
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Value (pvalueOf, pforgetPositive)
import Plutarch.LedgerApi.Value qualified as Value 
import Plutarch.LedgerApi.V2 hiding (PScriptContext)
import Plutarch.LedgerApi.V3 
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Airdrop.Utils (pdivCeil, pvalidityRangeStart, pletFieldsSpending, passert, pcontainsCurrencySymbols, pfindCurrencySymbolsByTokenPrefix, ptryOwnInput, ptryOwnOutput, phasCS, pand'List)
import Types.Constants (claimTokenTN, totalVestingInstallments)
import Scripts.AirdropMP (airdropTokenCS)
import Types.AirdropSet (PClaimValidatorConfig (..), PAirdropSetNode (..), PLNodeAction (..), PVestingDatum(..))
import Types.AirdropGlobalLogic (PAirdropGlobalLogicAction(..))
import Plutarch.Builtin (pforgetData)

tenBools :: Term s (PBuiltinList (PAsData PBool))
tenBools = foldr (\h t -> pcons # pconstantData h # t) pnil (replicate 10 True)

pisUniqueSet :: Term s (PBuiltinList (PAsData PInteger) :--> PBool)
pisUniqueSet = phoistAcyclic $ plam $ \xs -> perror  

pbuiltinListLength :: forall a s. Term s (PBuiltinList a :--> PInteger)
pbuiltinListLength = phoistAcyclic $ plam $ \xs ->
  pfix #$ plam $ \self acc l ->
    pelimList 
      (\_ ys -> self # (acc + 1) # ys)  -- cons case
      acc                               -- nil case
      l
    # 0
    # xs 

pAirdropGlobalLogicW :: Term s (PScriptContext :--> PUnit)
pAirdropGlobalLogicW = phoistAcyclic $ plam $ \ctx -> P.do
  ctxF <- pletFields @'["txInfo", "redeemer"] ctx
  let redeemer = punsafeCoerce @_ @_ @(PAsData PAirdropGlobalLogicAction) (pto ctxF.redeemer)
  redF <- pletFields @'["inputIdxs", "outputIdx"] redeemer

  infoF <- pletFields @'["inputs", "outputs", "redeemers"] ctxF.txInfo
  
  let checks = 
        pand'List 
          [ pconstant True
          ]

  pif checks (pconstant ()) perror 

pAirdropSetValidator ::
  ByteString ->
  ClosedTerm (PClaimValidatorConfig :--> PScriptContext :--> PUnit)
pAirdropSetValidator prefix = plam $ \claimConfig ctx' -> P.do 
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
      claimConfigF <- pletFields @'["vestingPeriodStart", "vestingPeriodEnd"] claimConfig
      PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatch (pfield @"credential" # ownInputF.address)
      let ownOutput = ptryOwnOutput # info.outputs # ownValHash
      ownOutputF <- pletFields @'["value", "datum"] ownOutput
      POutputDatum ((pfield @"outputDatum" #) -> ownOutputDatum) <- pmatch ownOutputF.datum
       
      PDJust ((pfield @"_0" #) -> ownInDat) <- pmatch scriptInfoF._1
      ownInputDatumF <- pletFields @'["extraData"] (punsafeCoerce @_ @_ @PAirdropSetNode (pto ownInDat))
      vestingDatumF <- pletFields @'["beneficiary", "totalVestingQty"] (punsafeCoerce @_ @_ @PVestingDatum ownInputDatumF.extraData)
      PPubKeyCredential ((pfield @"_0" #) -> beneficiaryHash) <- pmatch (pfield @"credential" # vestingDatumF.beneficiary)
      let currentTimeApproximation = pfromData $ pvalidityRangeStart # info.validRange 
          vestingTimeRemaining = (pfromData claimConfigF.vestingPeriodEnd) - currentTimeApproximation
          vestingPeriodLength = (pfromData claimConfigF.vestingPeriodEnd) - (pfromData claimConfigF.vestingPeriodStart)
          timeBetweenInstallments = pdivCeil # vestingPeriodLength # totalVestingInstallments
          futureInstallments = pdivCeil # vestingTimeRemaining # timeBetweenInstallments
          expectedRemainingQty = pdivCeil # (futureInstallments * vestingDatumF.totalVestingQty) # totalVestingInstallments

      ownInputVal <- plet $ pforgetPositive ownInputF.value
      inputVestingTokens <- plet $ pvalueOf # ownInputVal # pconstant airdropTokenCS # claimTokenTN 
      let vestingTokenDelta = expectedRemainingQty - inputVestingTokens 
          claimedTokens = Value.psingleton # pconstant airdropTokenCS # claimTokenTN # vestingTokenDelta
          checks = 
            pand'List
             [ (pto ownInDat) #== (pto ownOutputDatum)
             , pelem # beneficiaryHash # info.signatories
             , pfromData info.mint #== mempty
             , (ownInputVal <> claimedTokens) #== pforgetPositive ownOutputF.value
             ]
      pif checks 
          (pconstant ()) 
          perror 
  
-- TODO PlaceHolder # This contribution holds only the minimum amount of Ada + the FoldingFee, it cannot be updated. It cannot be removed until the reward fold has completed.