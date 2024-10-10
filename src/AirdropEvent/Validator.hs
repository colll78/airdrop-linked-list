{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AirdropEvent.Validator (pAirdropSetValidator, pAirdropGlobalLogicW) where 

import Data.ByteString (ByteString)

import Plutarch.LedgerApi.Value (pvalueOf, pforgetPositive)
import Plutarch.LedgerApi.Value qualified as Value 
import Plutarch.LedgerApi.V3 
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Airdrop.List (penforceNSpendRedeemers, pdropFast)
import Airdrop.Utils (pelemAtFast, pisScriptCredential, pdivCeil, pvalidityRangeStart, pletFieldsSpending, passert, pcontainsCurrencySymbols, pfindCurrencySymbolsByTokenPrefix, ptryOwnInput, pand'List)
import Types.Constants (claimTokenTN, totalVestingInstallments)
import Scripts.AirdropMP (airdropTokenCS)
import Types.AirdropSet (PClaimValidatorConfig (..), PAirdropSetNode (..), PLNodeAction (..), PVestingDatum(..))
import Types.AirdropGlobalLogic (PAirdropGlobalLogicAction(..), PAirdropGlobalLogicConfig(..))
import Plutarch.Builtin (pforgetData)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap

pfoldUTxOs :: (Term s (PAsData PTxOut) -> Term s (PAsData PTxOut) -> Term s PBool) -> Term s ((PBuiltinList (PAsData PTxOut)) :--> (PBuiltinList (PAsData PTxOut)) :--> PBool)
pfoldUTxOs func =
  pfix #$ plam $ \self la lb ->
    pelimList
      (\a as ->
        pelimList
          (\b bs -> 
            pif (func a b) 
                (self # as # bs) 
                (pconstant False)
          )
          perror
          lb
      )
      (pconstant True) 
      la

pprocessVestingPosition :: Term s PAddress -> Term s PInteger -> Term s (PAsData PTxOut) -> Term s (PAsData PTxOut) -> Term s PBool 
pprocessVestingPosition expectedAddress futureInstallments vestingInput vestingOutput = P.do 
  vestingInF <- pletFields @'["address", "value", "datum"] vestingInput 
  vestingOutF <- pletFields @'["value", "datum"] vestingOutput
  setNodeInDatum' <- plet $ vestingInF.datum
  POutputDatum ((pfield @"outputDatum" #) -> setNodeDatum) <- pmatch setNodeInDatum'
  setNodeDatumF <- pletFields @'["extraData"] (punsafeCoerce @_ @_ @PAirdropSetNode (pto setNodeDatum))
  vestingDatumF <- pletFields @'["beneficiary", "totalVestingQty"] (punsafeCoerce @_ @_ @PVestingDatum setNodeDatumF.extraData)
  let expectedRemainingQty = pdivCeil # (futureInstallments * vestingDatumF.totalVestingQty) # totalVestingInstallments
  vestingInVal <- plet $ pforgetPositive vestingInF.value
  inputVestingTokens <- plet $ pvalueOf # vestingInVal # pairdropCS # claimTokenTN
  let vestingTokenDelta = expectedRemainingQty - inputVestingTokens 
      claimedTokens = Value.psingleton # pairdropCS # claimTokenTN # vestingTokenDelta

  pand'List 
    [ setNodeInDatum' #== vestingOutF.datum
    , (vestingInVal <> claimedTokens) #== pforgetPositive vestingOutF.value
    , vestingInF.address #== expectedAddress
    ]

pairdropCS :: ClosedTerm PCurrencySymbol
pairdropCS = pconstant airdropTokenCS

pAirdropGlobalLogicW :: Term s (PAirdropGlobalLogicConfig :--> PScriptContext :--> PUnit)
pAirdropGlobalLogicW = phoistAcyclic $ plam $ \glConfig ctx -> P.do
  ctxF <- pletFields @'["txInfo", "redeemer"] ctx
  let redeemer = punsafeCoerce @_ @_ @(PAirdropGlobalLogicAction) (pto ctxF.redeemer)
  redF <- pletFields @'["inputIdxs", "outputIdx", "numProcessed"] redeemer
  infoF <- pletFields @'["inputs", "outputs", "redeemers", "validRange"] ctxF.txInfo
  
  -- type signature should be:
  inputsElemAt <- plet $ ((pelemAtFast # infoF.inputs) :: Term _ (PInteger :--> (PAsData PTxInInfo))) 
  let vestingInputs = pmap @PBuiltinList # plam (\i -> pfield @"resolved" # (inputsElemAt # pfromData i)) # redF.inputIdxs
      vestingOutputs = pdropFast # (pfromData redF.outputIdx) # infoF.outputs

  cfgF <- pletFields @'["vestingPeriodStart", "vestingPeriodEnd", "timeBetweenInstallments"] glConfig
  vestingEnd <- plet $ pfromData cfgF.vestingPeriodEnd 
  let currentTimeApproximation = pfromData $ pvalidityRangeStart # infoF.validRange
      vestingTimeRemaining = vestingEnd - currentTimeApproximation
  futureInstallments <- plet $ pdivCeil # vestingTimeRemaining # pfromData cfgF.timeBetweenInstallments

  vestingAddress <- plet $ pfield @"address" # (phead # vestingInputs)

  let vestingFirstCredential = pfield @"credential" # vestingAddress
      processFunc = pprocessVestingPosition vestingAddress futureInstallments
      processResult = (pfoldUTxOs processFunc) # vestingInputs # vestingOutputs

  let checks = 
        pand'List 
          [ pisScriptCredential vestingFirstCredential
          , penforceNSpendRedeemers (pfromData redF.numProcessed) infoF.redeemers
          , processResult
          ]

  pif checks (pconstant ()) perror 

pAirdropSetValidator ::
  ByteString ->
  ClosedTerm (PClaimValidatorConfig :--> PScriptContext :--> PUnit)
pAirdropSetValidator prefix = plam $ \claimConfig ctx' -> P.do 
  ctx <- pletFields @'["txInfo", "scriptInfo", "redeemer"] ctx'
  let redeemer = punsafeCoerce @_ @_ @PLNodeAction (pto ctx.redeemer)
  pmatch redeemer $ \case
    PPartialUnlock _ ->
      let stakeCerts = pfield @"wdrl" # ctx.txInfo 
          stakeScript = pfromData $ pfield @"globalCred" # claimConfig 
        in pmatch (AssocMap.plookup # stakeScript # stakeCerts) $ \case 
            PJust _ -> (pconstant ()) 
            PNothing -> perror 
    PLLinkedListAct _ -> P.do 
      scriptInfoF <- pletFieldsSpending (pforgetData ctx.scriptInfo)
      ownInputRef <- plet scriptInfoF._0

      info <- pletFields @'["inputs", "outputs", "mint", "validRange", "signatories", "referenceInputs"] ctx.txInfo
      txInputs <- plet info.inputs

      let ownInput = ptryOwnInput # txInputs # ownInputRef
            
      ownInputF <- pletFields @'["value", "address"] ownInput

      let ownInputValue = pfromData ownInputF.value

      -- all those CSs has tokens that prefixed by Node prefix
      -- any of those can be actual Node CS
      let potentialNodeCSs = pfindCurrencySymbolsByTokenPrefix # ownInputValue # pconstant prefix
      passert
        "Must mint/burn for any linked list interaction"
        (pcontainsCurrencySymbols # pfromData info.mint # potentialNodeCSs)
      (pconstant ())

  
-- TODO PlaceHolder # This contribution holds only the minimum amount of Ada + the FoldingFee, it cannot be updated. It cannot be removed until the reward fold has completed.