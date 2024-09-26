{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module AirdropEvent.Mint.Standard (
  mkAirdropNodeMP,
  mkAirdropNodeMPW,
) where

import Plutarch.LedgerApi.V3
import Plutarch.LedgerApi.Interval (pafter, pbefore)

--  pRemoveAndDeinit,

import Plutarch.Internal (Config (..))
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)
import AirdropEvent.Mint.Common (
  PPriceDiscoveryCommon (mint, ownCS),
  makeCommon,
  pDeinit,
  pInit,
  pInsert,
  pRemove,
 )

import Plutarch.Prelude
import Airdrop.Utils (pand'List, passert, pcond, pisFinite, phasUTxO, pintToByteString)
import Types.AirdropSet (PAirdropConfig (..), PAirdropNodeAction (..), PSignatureType(..), PVestingDatum(..))
import Types.Constants (claimRoot, airdropOperator)
import Airdrop.Crypto (pethereumPubKeyToPubKeyHash, pcompressPublicKey)
import Plutarch.Builtin (pserialiseData, pforgetData, PDataNewtype(..))
import MerkleTree.MerklePatriciaForestry (phas)
import Plutarch.Crypto (pverifyEcdsaSecp256k1Signature, pblake2b_256)

--------------------------------
-- FinSet Node Minting Policy:
--------------------------------

mkAirdropNodeMP ::
  Config ->
  ClosedTerm
    ( PAirdropConfig
        :--> PAirdropNodeAction
        :--> PScriptContext
        :--> PUnit
    )
mkAirdropNodeMP cfg = plam $ \claimConfig redm ctx -> P.do

  (common, inputs, sigs, vrange) <-
    runTermCont $
      makeCommon cfg ctx

  pmatch redm $ \case
    PLInit _ -> P.do
      claimConfigF <- pletFields @'["initUTxO"] claimConfig
      passert "Init must consume TxOutRef" $
        phasUTxO # claimConfigF.initUTxO # inputs
      pInit cfg common
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
      EcdsaSecpSignature claimData <- pmatch act.claimData 
      claimDataF <- pletFields @'["signature", "amount", "claimAddr", "proof"] claimData
      claimAddress <- plet $ claimDataF.claimAddr 
      let expectedVesting = pdata $ pcon $ PVestingDatum $ 
            pdcons @"beneficiary" # claimAddress
              #$ pdcons @"totalVestingQty" # claimDataF.amount
              #$ pdnil   
          msg = pblake2b_256 #$ pserialiseData # pforgetData claimAddress
          eth_compressed_pub_key = pcompressPublicKey pkToInsert

      pkhToInsert <- plet $ pethereumPubKeyToPubKeyHash # pkToInsert
      let insertChecks =
            pand'List
              [ pisFinite # vrange
              , pafter # claimConfigF.claimDeadline # vrange
              , (pverifyEcdsaSecp256k1Signature # eth_compressed_pub_key # msg # claimDataF.signature)
              , phas # claimRoot # pkhToInsert # (pintToByteString # claimDataF.amount) # claimDataF.proof
              ]
      pif insertChecks (pInsert cfg common # pdata pkhToInsert # expectedVesting) perror
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
  Config ->
  ClosedTerm
    ( PAirdropConfig
        :--> PScriptContext :--> PUnit 
    )
mkAirdropNodeMPW cfg = phoistAcyclic $ plam $ \claimConfig ctx ->
  let red = punsafeCoerce @_ @_ @PAirdropNodeAction (pto (pfield @"redeemer" # ctx))
   in mkAirdropNodeMP cfg # claimConfig # red # ctx
