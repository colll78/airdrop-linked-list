module AirdropEvent.Mint.Common (
  PPriceDiscoveryCommon (..),
  makeCommon,
  pInit,
  pDeinit,
  pRemove,
  pInsert,
) where

import Plutarch.LedgerApi.Value (plovelaceValueOf, pnormalize, pvalueOf)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Interval (pafter, pbefore)
import Plutarch.TermCont (pguardC)
import Plutarch.Internal (Config (..))
import Plutarch.List (pconvertLists)
import Plutarch.Monadic qualified as P
import Plutarch.Positive (PPositive)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (CurrencySymbol)
import Airdrop.Utils (
  pand'List,
  passert,
  paysToAddress,
  paysToCredential,
  pcheck,
  pcountOfUniqueTokens,
  pfindWithRest,
  pfindCurrencySymbolsByTokenPrefix,
  pheadSingleton,
  phasCS,
  phasDataCS,
  pmapMaybe,
  pmustFind,
  psingletonOfCS,
  pvalueOfOne,
  (#>),
  (#>=),
  pfromPDatum, 
  ptryFromInlineDatum,
  pnonew,
  pmapFilter,
 )
import Types.Constants (minAda, nodeDepositAda, minAdaToCommit, pnodeKeyTN, poriginNodeTN, pparseNodeKey)
import Types.AirdropSet
import Plutarch.LedgerApi.V3 
import Plutarch.Builtin (PDataNewtype(..), pforgetData)
import Airdrop.Crypto

{- | Ensures that the minted amount of the FinSet CS is exactly the specified
     tokenName and amount
-}
correctNodeTokenMinted ::
  ClosedTerm
    ( PCurrencySymbol
        :--> PTokenName
        :--> PInteger
        :--> PValue 'Sorted 'NonZero
        :--> PBool
    )
correctNodeTokenMinted = phoistAcyclic $
  plam $ \nodeCS tokenName amount mint -> P.do
    PJust nodeMint <- pmatch $ AssocMap.plookup # nodeCS # pto mint
    let tokenMap = AssocMap.psingleton # tokenName # amount
    tokenMap #== nodeMint

nodeInputUtxoDatum ::
  ClosedTerm
    ( PAsData PCurrencySymbol
        :--> PTxOut
        :--> PMaybe (PAsData PAirdropSetNode)
    )
nodeInputUtxoDatum = phoistAcyclic $
  plam $ \nodeCS out -> P.do
    txOut <- pletFields @'["datum", "value"] out
    let value = pfromData txOut.value
    pcheck (phasDataCS # nodeCS # value) $
      punsafeCoerce $
        ptryFromInlineDatum # txOut.datum

nodeInputUtxoDatumUnsafe ::
  ClosedTerm
    ( PTxOut
        :--> PPair (PValue 'Sorted 'Positive) (PAsData PAirdropSetNode)
    )
nodeInputUtxoDatumUnsafe = phoistAcyclic $
  plam $ \out -> pletFields @'["value", "datum"] out $ \outF ->
    plet (punsafeCoerce $ ptryFromInlineDatum # outF.datum) $ \nodeDat ->
      pcon (PPair (pfromData outF.value) nodeDat)

parseNodeOutputUtxo ::
  Config ->
  ClosedTerm
    ( PAsData PCurrencySymbol
        :--> PTxOut
        :--> PPair (PValue 'Sorted 'Positive) (PAsData PAirdropSetNode)
    )
parseNodeOutputUtxo cfg = phoistAcyclic $
  plam $ \nodeCS out -> P.do
    txOut <- pletFields @'["address", "value", "datum"] out
    value <- plet $ pfromData $ txOut.value
    PPair tn amount <- pmatch $ psingletonOfCS # nodeCS # value
    POutputDatum od <- pmatch $ pfromData $ txOut.datum
    datum <- plet $ pfromPDatum #$ pfield @"outputDatum" # od
    let nodeKey = pparseNodeKey # tn
        datumKey = pmatch (pfield @"key" # datum) $ \case
          PEmpty _ -> pcon PNothing
          PKey ((pfield @"_0" #) -> key) -> pcon $ PJust key

    -- Prevents TokenDust attack
    passert "All FSN tokens from node policy" $ 
      pheadSingleton # (pfindCurrencySymbolsByTokenPrefix # value # pconstant "FSN") #== nodeCS
    passert "Too many assets" $ pcountOfUniqueTokens # value #== 2
    passert "Incorrect number of nodeTokens" $ amount #== 1
    passert "node is not ordered" $ validNode # datum
    passert "Incorrect token name" $ nodeKey #== datumKey
    passert "Does not hold required Ada" $
      plovelaceValueOf # value #>= minAdaToCommit
    pcon (PPair value datum)

makeCommon ::
  forall {r :: PType} {s :: S}.
  Config ->
  Term s PScriptContext ->
  TermCont @r
    s
    ( PPriceDiscoveryCommon s
    , Term s (PBuiltinList PTxInInfo)
    , Term s (PBuiltinList (PAsData PPubKeyHash))
    , Term s (PInterval PPosixTime)
    )
makeCommon cfg ctx' = do
  ------------------------------
  -- Preparing info needed for validation:
  ctx <- tcont $ pletFields @'["txInfo", "scriptInfo"] ctx'
  info <-
    tcont $
      pletFields
        @'["inputs", "outputs", "mint", "referenceInputs", "signatories", "validRange"]
        ctx.txInfo

  ownCS <- tcont . plet $ P.do
    PMintingScript mintRecord <- pmatch $ ctx.scriptInfo
    pfield @"_0" # mintRecord

  mint <- tcont . plet $ pnormalize #$ pfromData info.mint
  -- asOuts <- tcont . plet $ pmap # plam (pfield @"resolved" #)
  -- refInsAsOuts <- tcont . plet $ asOuts # pfromData info.referenceInputs
  hasNodeTk <- tcont . plet $ phasDataCS # ownCS
  -- insAsOuts <- tcont . plet $ pmap # plam (pfield @"resolved" #) # info.inputs
  -- onlyAtNodeVal <- tcont . plet $ pfilter @PBuiltinList # plam (\txo -> (hasNodeTk # (pfield @"value" # txo)))
  txInputs <- tcont . plet $ punsafeCoerce @_ @_ @(PBuiltinList PTxInInfo) info.inputs 
  let txOutputs = punsafeCoerce @_ @_ @(PBuiltinList PTxOut) info.outputs 
  fromNodeValidator <- tcont . plet $ pmapFilter @PBuiltinList # plam (\txo -> (hasNodeTk # (pfield @"value" # txo))) # plam (pfield @"resolved" #) # txInputs
  toNodeValidator <- tcont . plet $ pfilter @PBuiltinList # plam (\txo -> (hasNodeTk # (pfield @"value" # txo))) # txOutputs
  ------------------------------

  let atNodeValidator =
        pelimList
          ( \x xs -> plet (paysToAddress # (pfield @"address" # x)) $ \isSameAddr ->
              pand'List
                [ (pall # isSameAddr # xs)
                , (pall # isSameAddr # toNodeValidator)
                ]
          )
          (pconstant True)
          fromNodeValidator

  pguardC "all same origin" atNodeValidator

  nodeInputs <- tcont . plet $ pmap # nodeInputUtxoDatumUnsafe #$ pconvertLists # fromNodeValidator

  nodeOutputs <-
    tcont . plet $
      pmap
        # (parseNodeOutputUtxo cfg # ownCS)
        #$ pconvertLists
        # toNodeValidator

  let common =
        MkCommon
          { ownCS = (pfromData ownCS)
          , mint
          , nodeInputs
          , nodeOutputs
          }
  vrange <- tcont . plet $ pfromData info.validRange
  pure
    ( common
    , txInputs 
    , info.signatories
    , vrange
    )

pInit :: forall (s :: S). Config -> PPriceDiscoveryCommon s -> Term s PUnit
pInit cfg common = P.do
  -- Input Checks
  passert "Init must not spend Nodes" $ pnull # common.nodeInputs
  -- Output Checks:
  PPair _ otherNodes <-
    pmatch $
      pfindWithRest # plam (\nodePair -> pmatch nodePair (\(PPair _ nodeDat) -> isEmptySet # nodeDat)) # common.nodeOutputs

  passert "Init output exactly one Node" $
    pnull # otherNodes
  -- Mint checks:
  passert "Incorrect mint for Init" $
    correctNodeTokenMinted # common.ownCS # poriginNodeTN # 1 # common.mint

  pconstant ()

-- TODO add deadline check
pDeinit :: forall s. PPriceDiscoveryCommon s -> Term s PUnit
pDeinit common = P.do
  -- Input Checks

  -- The following code should be used instead for protocols where node removal
  -- needs to preserve the integrity of the linked list.
  -- PPair _ otherNodes <- pmatch $ pfindWithRest # plam (\nodePair -> pmatch nodePair (\(PPair _ dat) -> isEmptySet # dat)) # common.nodeInputs
  
  -- If order does not need to be preserved for head removal:
  PPair _ otherNodes <- pmatch $ pfindWithRest # plam (\nodePair -> pmatch nodePair (\(PPair _ dat) -> isNothing # (pfield @"key" # dat))) # common.nodeInputs
  
  passert "Deinit must spend exactly one node" $ pnull # otherNodes
  
  -- Output Checks:
  passert "Deinit must not output nodes" $ pnull # common.nodeOutputs

  -- Mint checks:
  passert "Incorrect mint for DeInit" $
    correctNodeTokenMinted # common.ownCS # poriginNodeTN # (-1) # common.mint

  pconstant ()

pInsert ::
  forall (s :: S).
  Config ->
  PPriceDiscoveryCommon s ->
  Term s (PAsData PByteString :--> PAsData PVestingDatum :--> PUnit)
pInsert cfg common = plam $ \pkToInsert expectedDatum -> P.do
  keyToInsert <- plet $ pfromData pkToInsert 
  
  -- Input Checks:
  -- There is only one spent node (tx inputs contains only one node UTxO)
  -- The spent node indeed covers the key we want to insert
  PPair coveringValue coveringDatum <- pmatch $ pheadSingleton # common.nodeInputs
  passert "Spent node should cover inserting key" $ coversLiquidityKey # coveringDatum # keyToInsert
  
  -- Output Checks:
  coveringDatumF <- pletFields @'["key", "next", "extraData"] coveringDatum 

  nodeKeyToInsert <- plet $ pcon $ PKey $ pdcons @"_0" # pdata keyToInsert #$ pdnil
  isInsertedOnNode <- plet $ pisInsertedOnNode # nodeKeyToInsert # coveringDatumF.key # coveringDatumF.extraData 
  isInsertedNode <- plet $ pisInsertedNode # nodeKeyToInsert # coveringDatumF.next # pforgetData expectedDatum 

  passert "Incorrect node outputs for Insert" $
    pany
      # plam (\nodePair -> pmatch nodePair (\(PPair val dat) -> val #== coveringValue #&& isInsertedOnNode # dat))
      # common.nodeOutputs
      #&& pany # plam (\nodePair -> pmatch nodePair (\(PPair _ dat) -> isInsertedNode # dat)) # common.nodeOutputs 

  -- Mint checks:
  passert "Incorrect mint for Insert" $
    correctNodeTokenMinted # common.ownCS # (pnodeKeyTN # keyToInsert) # 1 # common.mint

  pconstant ()

pRemove ::
  forall (s :: S).
  PPriceDiscoveryCommon s ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s (PAsData PByteString :--> PUnit)
pRemove common sigs = plam $ \pkToRemove -> P.do
  keyToRemove <- plet $ pfromData pkToRemove
  -- Input Checks
  PPair removedValue removedDatum <- pmatch (pheadSingleton # common.nodeInputs)

  removedDatumF <- pletFields @'["key", "next", "extraData"] removedDatum 
  vestingDatumF <- pletFields @'["beneficiary"] (punsafeCoerce @_ @_ @PVestingDatum removedDatumF.extraData)
  
  PPubKeyCredential ((pfield @"_0" #) -> beneficiaryHash) <- pmatch (pfield @"credential" # vestingDatumF.beneficiary)
  
  -- Enable if you want to allow scripts to claim the airdrop
  -- Also must check that either the ownerCred signs the tx or
  -- the associated script is invoked in the tx (ie cred in txInfoWithdrawals or cs in txInfoMint) 
  -- let credential = pfield @"credential" # vestingDatumF.beneficiary 
  --     isAuthed = pmatch credential $ \case 
  --                   PPubKeyCredential (pfield @"_0" #) -> pelem # pkh # sigs 
  --                   PScriptCredential ((pfield @"_0" #) -> 
  --                     (pfstBuiltin # (phead # pto ctxF.wdrl)) 
  --                       #== pdata $ pcon $ PStakingHash $ pdcons @"_0" # (pcon $ PScriptCredential $ pdcons @"_0" # validatorHash # pdnil) # pdnil             

  nodeToRemoveTN <- plet (pnodeKeyTN # keyToRemove)

  passert "Incorrect node UTxO for Remove" $
    pvalueOf # removedValue # common.ownCS # nodeToRemoveTN #== 1

  passert "Incorrect mint for Remove" $
    correctNodeTokenMinted # common.ownCS # nodeToRemoveTN # (-1) # common.mint

  passert "signed by user." (pelem # beneficiaryHash # sigs)

  pconstant ()

-- Common information shared between all redeemers.
data PPriceDiscoveryCommon (s :: S) = MkCommon
  { ownCS :: Term s PCurrencySymbol
  -- ^ state token (own) CS
  , mint :: Term s (PValue 'Sorted 'NonZero)
  -- ^ value minted in current Tx
  , nodeInputs :: Term s (PList (PPair (PValue 'Sorted 'Positive) (PAsData PAirdropSetNode)))
  -- ^ current Tx outputs to AuctionValidator
  , nodeOutputs :: Term s (PList (PPair (PValue 'Sorted 'Positive) (PAsData PAirdropSetNode)))
  -- ^ current Tx inputs
  }
  deriving stock (Generic)
