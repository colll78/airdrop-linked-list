module LiquidityEvent.Mint.Common (
  PPriceDiscoveryCommon (..),
  makeCommon,
  pInit,
  pDeinit,
  pRemove,
  pInsert,
  pClaim
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
import PriceDiscoveryEvent.Utils (
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
import Types.Constants (minAda, nodeDepositAda, minAdaToCommit, pcorrNodeTN, pnodeKeyTN, poriginNodeTN, pparseNodeKey)
import Types.LiquiditySet
import Plutarch.LedgerApi.V3 

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

pdivideCeil :: Term s (PInteger :--> PInteger :--> PInteger)
pdivideCeil = phoistAcyclic $ plam $ \a b -> (pdiv # a # b) + pif ((pmod # a # b) #> 0) 1 0

nodeInputUtxoDatum ::
  ClosedTerm
    ( PAsData PCurrencySymbol
        :--> PTxOut
        :--> PMaybe (PAsData PLiquiditySetNode)
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
        :--> PPair (PValue 'Sorted 'Positive) (PAsData PLiquiditySetNode)
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
        :--> PPair (PValue 'Sorted 'Positive) (PAsData PLiquiditySetNode)
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
    , Term s (PBuiltinList PTxOut)
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
  txOutputs <- tcont . plet $ punsafeCoerce @_ @_ @(PBuiltinList PTxOut) info.outputs 
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
    , txOutputs 
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
pDeinit :: forall s. Config -> PPriceDiscoveryCommon s -> Term s PUnit
pDeinit cfg common = P.do
  -- Input Checks
  -- The following commented code should be used instead for protocols where node removal
  -- needs to preserve the integrity of the linked list.
  PPair _ otherNodes <- pmatch $ pfindWithRest # plam (\nodePair -> pmatch nodePair (\(PPair _ dat) -> isEmptySet # dat)) # common.nodeInputs
  -- PPair _ otherNodes <- pmatch $ pfindWithRest # plam (\nodePair -> pmatch nodePair (\(PPair _ dat) -> isNothing # (pfield @"key" # dat))) # common.nodeInputs
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
  Term s (PAsData PPubKeyHash :--> PAsData PLiquiditySetNode :--> PUnit)
pInsert cfg common = plam $ \pkToInsert node -> P.do
  keyToInsert <- plet $ pnonew $ pfromData pkToInsert 
  
  -- Input Checks:
  -- There is only one spent node (tx inputs contains only one node UTxO)
  -- The spent node indeed covers the key we want to insert
  PPair coveringValue coveringDatum <- pmatch $ pheadSingleton # common.nodeInputs
  passert "Spent node should cover inserting key" $ coversLiquidityKey # coveringDatum # keyToInsert

  -- Output Checks:
  coveringDatumF <- pletFields @'["key", "next"] coveringDatum 

  nodeKeyToInsert <- plet $ pcon $ PKey $ pdcons @"_0" # pdata keyToInsert #$ pdnil
  isInsertedOnNode <- plet $ pisInsertedOnNode # nodeKeyToInsert # coveringDatumF.key 
  isInsertedNode <- plet $ pisInsertedNode # nodeKeyToInsert # coveringDatumF.next 

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
  Config ->
  PPriceDiscoveryCommon s ->
  Term s (PInterval PPosixTime) ->
  Term s PLiquidityConfig ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s (PAsData PPubKeyHash :--> PAsData PLiquiditySetNode :--> PUnit)
pRemove cfg common vrange discConfig outs sigs = plam $ \pkToRemove node -> P.do
  perror 

pClaim ::
  forall (s :: S).
  Config ->
  PPriceDiscoveryCommon s ->  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s (PAsData PPubKeyHash :--> PUnit)
pClaim cfg common outs sigs = plam $ \pkToRemove -> P.do
  keyToRemove <- plet $ pnonew $ pfromData pkToRemove
  
  -- Input Checks
  PPair removedValue _removedDatum <- pmatch (pheadSingleton # common.nodeInputs)

  nodeToRemoveTN <- plet (pnodeKeyTN # keyToRemove)

  passert "Incorrect node UTxO for Remove" $
    pvalueOf # removedValue # common.ownCS # nodeToRemoveTN #== 1

  passert "Incorrect mint for Remove" $
    correctNodeTokenMinted # common.ownCS # nodeToRemoveTN # (-1) # common.mint

  passert "signed by user." (pelem # pkToRemove # sigs)
          
  -- verify that this node has been processed by the rewards fold by checking that count of tokens is 3. 
  passert "Claim broke phase rules." (pcountOfUniqueTokens # removedValue #>= 3)

  pconstant ()

-- Common information shared between all redeemers.
data PPriceDiscoveryCommon (s :: S) = MkCommon
  { ownCS :: Term s PCurrencySymbol
  -- ^ state token (own) CS
  , mint :: Term s (PValue 'Sorted 'NonZero)
  -- ^ value minted in current Tx
  , nodeInputs :: Term s (PList (PPair (PValue 'Sorted 'Positive) (PAsData PLiquiditySetNode)))
  -- ^ current Tx outputs to AuctionValidator
  , nodeOutputs :: Term s (PList (PPair (PValue 'Sorted 'Positive) (PAsData PLiquiditySetNode)))
  -- ^ current Tx inputs
  }
  deriving stock (Generic)
