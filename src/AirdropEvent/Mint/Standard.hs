{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module AirdropEvent.Mint.Standard (
  mkAirdropNodeMP,
  mkAirdropNodeMPW,
) where

import Plutarch.LedgerApi.V3
import Plutarch.LedgerApi.Interval (pafter, pbefore)

--  pRemoveAndDeinit,
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)
import AirdropEvent.Mint.Common (
  makeCommon,
  pDeinit,
  pInit,
  pInsert,
  pRemove,
 )

import Plutarch.Prelude
import Airdrop.Utils (pand'List, passert, pcond, pisFinite, phasUTxO, pintToByteString)
import Types.AirdropSet (PAirdropConfig (..), PAirdropNodeAction (..), PSignatureType(..), PVestingDatum(..), PNetwork(..), pletFieldsSignature)
import Types.Constants (claimRoot, airdropOperator)
import Airdrop.Crypto (pcardanoPubKeyToPubKeyHash, pethereumPubKeyToPubKeyHash, pcompressPublicKey)
import Plutarch.Builtin (pserialiseData, pforgetData, PDataNewtype(..))
import MerkleTree.MerklePatriciaForestry (phas)
import Plutarch.Crypto (pverifyEcdsaSecp256k1Signature, pblake2b_256)

--------------------------------
-- FinSet Node Minting Policy:
--------------------------------

mkAirdropNodeMP ::
  ClosedTerm
    ( PAirdropConfig
        :--> PAirdropNodeAction
        :--> PScriptContext
        :--> PUnit
    )
mkAirdropNodeMP = plam $ \claimConfig redm ctx -> P.do

  (common, inputs, sigs, vrange) <-
    runTermCont $
      makeCommon ctx

  pmatch redm $ \case
    PLInit _ -> P.do
      claimConfigF <- pletFields @'["initUTxO"] claimConfig
      passert "Init must consume TxOutRef" $
        phasUTxO # claimConfigF.initUTxO # inputs
      pInit common
    PLDeinit _ -> P.do
      claimConfigF <- pletFields @'["vestingPeriodEnd"] claimConfig
      passert "vrange not finite" (pisFinite # vrange)
      passert "vrange not finite" (pelem # airdropOperator # sigs)
      vestingEnd <- plet (pcon $ PPosixTime $ pcon $ PDataNewtype $ claimConfigF.vestingPeriodEnd)
      pcond           
        [ ((pbefore # vestingEnd # vrange), (pDeinit common))
        ]
        perror 
    PLInsert action -> P.do
      claimConfigF <- pletFields @'["claimDeadline"] claimConfig
      act <- pletFields @'["keyToInsert", "claimData"] action
      pkToInsert <- plet act.keyToInsert
      actClaimData <- plet act.claimData
      -- @'["signature", "amount", "claimAddr", "proof", "network"]
      claimDataF <- pletFieldsSignature (pforgetData actClaimData)
      claimAddress <- plet $ claimDataF.claimAddr 
      claimAmount <- plet $ claimDataF.amount
      let expectedVesting = pdata $ pcon $ PVestingDatum $ 
            pdcons @"beneficiary" # claimAddress
              #$ pdcons @"totalVestingQty" # claimAmount
              #$ pdnil  
          msg = pblake2b_256 #$ pserialiseData # pforgetData claimAddress
      pmatch (pfromData actClaimData) $ \case 
        Ed25519Signature _ -> P.do 
          (signatureCheck, pkhToInsert) <- runTermCont $ do 
            networkId <- tcont $ pmatch claimDataF.network
            case networkId of 
              PCardano -> do
                pkhToInsert <- tcont $ plet $ pcardanoPubKeyToPubKeyHash # pkToInsert
                let pkhToCheck = pdata $ pcon $ PPubKeyHash $ pcon $ PDataNewtype $ pdata $ pkhToInsert
                pure ((pelem # pkhToCheck # sigs), pkhToInsert)
              _ -> do
                pure (perror, perror)  
          let insertChecks =
                pand'List
                  [ pisFinite # vrange
                  , pafter # claimConfigF.claimDeadline # vrange
                  , signatureCheck
                  , phas # claimRoot # pkhToInsert # (pintToByteString # pfromData claimAmount) # claimDataF.proof
                  ]
          pif insertChecks (pInsert common # pdata pkhToInsert # expectedVesting) perror       
        SchnorrSignature _claimData -> P.do 
          perror 
        EcdsaSecpSignature _ -> P.do 
          let eth_compressed_pub_key = pcompressPublicKey pkToInsert

          pkhToInsert <- plet $ pethereumPubKeyToPubKeyHash # pkToInsert
          let insertChecks =
                pand'List
                  [ pisFinite # vrange
                  , pafter # claimConfigF.claimDeadline # vrange
                  , (pverifyEcdsaSecp256k1Signature # eth_compressed_pub_key # msg # claimDataF.signature)
                  , phas # claimRoot # pkhToInsert # (pintToByteString # pfromData claimAmount) # claimDataF.proof
                  ]
          pif insertChecks (pInsert common # pdata pkhToInsert # expectedVesting) perror


    PLRemove action -> P.do 
      claimConfigF <- pletFields @'["vestingPeriodEnd"] claimConfig
      act <- pletFields @'["keyToRemove"] action
      vestingEnd <- plet (pcon $ PPosixTime $ pcon $ PDataNewtype $ claimConfigF.vestingPeriodEnd)
      passert "vrange not finite" (pisFinite # vrange)
      pcond 
        [ ((pbefore # vestingEnd # vrange), (pRemove common sigs # act.keyToRemove))
        ]
        perror 

mkAirdropNodeMPW ::
  ClosedTerm
    ( PAirdropConfig
        :--> PScriptContext :--> PUnit 
    )
mkAirdropNodeMPW = phoistAcyclic $ plam $ \claimConfig ctx ->
  let red = punsafeCoerce @_ @_ @PAirdropNodeAction (pto (pfield @"redeemer" # ctx))
   in mkAirdropNodeMP # claimConfig # red # ctx
