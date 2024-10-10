{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Testing.AirdropValidator (tests, dumpDebug) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Airdrop.Utils (pand'List, pintToByteString)
import Testing.Eval (psucceeds, passert, toBuiltinHexString)
import Plutarch.Crypto 
import Airdrop.Crypto (pethereumPubKeyToPubKeyHash, pcompressPublicKey)
import Plutarch.Prelude
import PlutusCore.Crypto.Hash qualified as Hash
import PlutusLedgerApi.V3
import PlutusLedgerApi.V1.Value qualified as Value 
import PlutusTx.AssocMap qualified as AssocMap
import Types.AirdropGlobalLogic (AirdropGlobalLogicAction(..), AirdropGlobalLogicConfig(..))
import AirdropEvent.Validator
import Types.Constants (claimTokenTN, totalVestingInstallments)
import Scripts.NodeMP (airdropNodeCS)
import Scripts.AirdropMP (airdropTokenCS)
import Types.AirdropSet (VestingDatum(..), AirdropSetNode(..), NodeKey(..), LNodeAction(..))
import Data.ByteString (ByteString)
import Prettyprinter (Pretty (pretty), layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.String (renderString)
import Data.List (findIndices, sortOn)
import PlutusTx.Eq qualified

tests :: TestTree
tests = testGroup "AirdropGlobalValidator Tests"
  [ testCase "Bulk Claim" $
     let config = airdropGlobalConfig (getPOSIXTime mockVestingStart) (getPOSIXTime mockVestingEnd)
     in psucceeds $
          pAirdropGlobalLogicW 
            # (pconstant config) 
            # (pconstant $ succeedsAirdropGlobalScriptContext config)
  ]

instance PlutusTx.Eq.Eq ScriptPurpose where
  (==) = (==)

divCeil :: Integral a => a -> a -> a
divCeil x y = 1 + div (x - 1) y

authCS :: CurrencySymbol
authCS = "deadbeef"

mockVestingStart :: POSIXTime
mockVestingStart = 1684449918000 -- May 19, 2023, 00:05:18 GMT

mockVestingEnd :: POSIXTime
mockVestingEnd = mockVestingStart + 30 * 24 * 60 * 60 * 1000 -- 30 days after start

mockCurrentTime :: POSIXTime
mockCurrentTime = mockVestingStart + 10 * 24 * 60 * 60 * 1000 -- 10 days after start

mockValidityRange :: Interval POSIXTime
mockValidityRange = Interval 
    { ivFrom = LowerBound (Finite mockCurrentTime) True
    , ivTo = UpperBound (Finite (mockCurrentTime + 60000)) False -- Valid for 1 minute
    }

globalLogicScriptContext :: ScriptContext 
globalLogicScriptContext = 
  ScriptContext 
    { scriptContextTxInfo = txInfoBase
    , scriptContextRedeemer = globalLogicRedeemer
    , scriptContextScriptInfo =  RewardingScript (ScriptCredential "deadbeef")
    }

globalLogicRedeemer :: Redeemer
globalLogicRedeemer = Redeemer {getRedeemer = toBuiltinData glAct}
  where 
    glAct = 
      AirdropGlobalLogicAction 
        { inputsIdxs = []
        , outputIdx = 0
        , numProcessed = 0
        }

airdropGlobalConfig :: Integer -> Integer -> AirdropGlobalLogicConfig
airdropGlobalConfig start end =
  let vestingPeriodLength = end - start 
      betweenTrenches = divCeil vestingPeriodLength (plift totalVestingInstallments)
  in 
    AirdropGlobalLogicConfig
      { vestingPeriodStart = start
      , vestingPeriodEnd = end
      , timeBetweenInstallments = betweenTrenches 
      }

txInfoBase :: TxInfo
txInfoBase =
  TxInfo
    { -- Every tx info must contain a reference to the authorised script we are testing.
      txInfoReferenceInputs = []
    , txInfoInputs = []
    , txInfoRedeemers = AssocMap.unsafeFromList []
    , txInfoOutputs = []
    , txInfoFee = Lovelace 700_000
    , txInfoMint = Value $ AssocMap.safeFromList []
    , txInfoTxCerts = []
    , txInfoWdrl = AssocMap.unsafeFromList []
    , txInfoValidRange = mockValidityRange
    , txInfoSignatories = []
    , txInfoData = AssocMap.safeFromList []
    , txInfoId = TxId ""
    , txInfoVotes                 = AssocMap.unsafeFromList []
    , txInfoProposalProcedures    = [] 
    , txInfoCurrentTreasuryAmount = Nothing 
    , txInfoTreasuryDonation      = Nothing
    }

mockAirdropValidatorAddress :: Address 
mockAirdropValidatorAddress = 
  Address
    { addressCredential = ScriptCredential "11111111111111111111111111111111111111111111111111111111"
    , addressStakingCredential = Nothing
    }

mockPKH :: PubKeyHash 
mockPKH = PubKeyHash $ toBuiltinHexString "deadbeef"

mockVestingDatum :: PubKeyHash -> Integer -> VestingDatum 
mockVestingDatum ownerPKH vestingQty =
  VestingDatum
    { beneficiary = Address {addressCredential = PubKeyCredential ownerPKH, addressStakingCredential = Nothing}
    , totalVestingQty = vestingQty 
    }

mockAirdropSetNodeDatum :: BuiltinByteString -> BuiltinByteString -> PubKeyHash -> Integer -> AirdropSetNode
mockAirdropSetNodeDatum key next beneficiary vestingQty = 
  let snKey = if key == "" then Empty else Key key
      snNext = if next == "" then Empty else Key next
  in 
    MkAirdropSetNode
      { key = snKey
      , next = snNext 
      , committed = mockVestingDatum beneficiary vestingQty 
      }

sortValue :: Value -> Value
sortValue (Value m) = Value $ AssocMap.unsafeFromList $ sortOn (csToBS . fst) $ map sortInner $ AssocMap.toList m
  where
    csToBS :: CurrencySymbol -> BuiltinByteString
    csToBS (CurrencySymbol bs) = bs

    sortInner (cs, innerMap) = (cs, AssocMap.unsafeFromList $ sortOn (tnToBS . fst) $ AssocMap.toList innerMap)
    
    tnToBS :: TokenName -> BuiltinByteString
    tnToBS (TokenName bs) = bs

airdropValidatorTxOut :: BuiltinByteString -> BuiltinByteString -> PubKeyHash ->  Integer -> Integer -> TxOut
airdropValidatorTxOut key next beneficiary totVestingQty remainVestingQty =
  TxOut
    { txOutAddress = mockAirdropValidatorAddress
    , txOutValue = 
        sortValue (Value.singleton adaSymbol adaToken 2_000_000 
          <> Value.singleton airdropNodeCS (TokenName $ "FSN" <> key) 1
          <> Value.singleton airdropTokenCS (plift claimTokenTN) remainVestingQty)
    , txOutDatum = OutputDatum ( Datum {getDatum = toBuiltinData $ mockAirdropSetNodeDatum key next beneficiary totVestingQty} )
    , txOutReferenceScript = Nothing
    }

airdropValidatorUTxO :: BuiltinByteString -> BuiltinByteString -> PubKeyHash -> Integer -> Integer -> TxInInfo
airdropValidatorUTxO key next beneficiary totVestingQty remainVestingQty = 
  TxInInfo 
    { txInInfoOutRef = TxOutRef{txOutRefId = "deadbeef", txOutRefIdx = 0}
    , txInInfoResolved = airdropValidatorTxOut key next beneficiary totVestingQty remainVestingQty
    }

calculateFutureInstallments :: POSIXTime -> Integer -> Integer -> Integer
calculateFutureInstallments (POSIXTime currentTime) vestingEnd betweenTrenches =
    let vestingTimeRemaining = vestingEnd - currentTime
        futureInstallments = divCeil vestingTimeRemaining betweenTrenches
    in max 0 futureInstallments

createTestTxInfo :: AirdropGlobalLogicConfig -> [TxInInfo] -> TxInfo
createTestTxInfo glConfig inputs =
    let betweenTrenches = timeBetweenInstallments glConfig
        (POSIXTime currentTime) = getLowerBoundTime (txInfoValidRange txInfoBase)
        vestingTimeRemaining = (vestingPeriodEnd glConfig) - currentTime
        futureInstallments = divCeil vestingTimeRemaining betweenTrenches
        
        processInput txIn =
            let input = txInInfoResolved txIn
                MkAirdropSetNode {key, next, committed = VestingDatum {beneficiary, totalVestingQty}} = 
                    unsafeFromBuiltinData $ getOutputDatum $ txOutDatum input
                -- remainingQty = Value.valueOf (txOutValue input) airdropTokenCS (plift claimTokenTN)
                expectedRemainingQty = (futureInstallments * totalVestingQty) `divCeil` (plift totalVestingInstallments)
            in airdropValidatorTxOut 
                (toPubKeyHash key)
                (toPubKeyHash next)
                (case addressCredential beneficiary of
                    PubKeyCredential pkh -> pkh
                    _ -> error "Expected PubKeyCredential")
                totalVestingQty
                expectedRemainingQty
        
        -- create the expected outputs
        outputs = map processInput inputs
        
        -- Create redeemers for each input (Spending) and one for the global logic (Rewarding)
        allRedeemers = map (\i -> (Spending (txInInfoOutRef i), Redeemer $ toBuiltinData PartialUnlock)) inputs
    
    in txInfoBase
        { txInfoInputs = inputs
        , txInfoOutputs = outputs
        , txInfoValidRange = mockValidityRange
        , txInfoRedeemers = AssocMap.unsafeFromList allRedeemers
        }
  where
    toPubKeyHash (Key bs) = bs
    toPubKeyHash Empty = ""
    
    getLowerBoundTime (Interval lower _) = 
        case lower of
            LowerBound (Finite time) _ -> time
            _ -> error "Invalid lower bound"

    getOutputDatum d = 
      case d of 
        OutputDatum d' -> getDatum d' 
        _ -> error "getOutputDatum: Expected InlineDatum"

mockInputs :: [TxInInfo]
mockInputs = 
  [ airdropValidatorUTxO "key1" "key2" mockPKH 15_000_000 15_000_000
  , airdropValidatorUTxO "key2" "key3" mockPKH 10_000_000 10_000_000
  , airdropValidatorUTxO "key5" "key6" mockPKH 23_000_000 23_000_000
  ]

succeedsAirdropGlobalScriptContext :: AirdropGlobalLogicConfig -> ScriptContext  
succeedsAirdropGlobalScriptContext conf = 
  let txInfo'@TxInfo{txInfoRedeemers=oldRedeemers} = createTestTxInfo conf mockInputs
      redeemer = createGlobalLogicRedeemer txInfo
      txInfo = txInfo' {txInfoRedeemers = AssocMap.insert (Rewarding (ScriptCredential "deadbeef")) (Redeemer $ toBuiltinData redeemer) oldRedeemers}
  in  ScriptContext
        { scriptContextTxInfo = txInfo
        , scriptContextRedeemer = redeemer
        , scriptContextScriptInfo = RewardingScript (ScriptCredential "deadbeef")
        }

createGlobalLogicRedeemer :: TxInfo -> Redeemer
createGlobalLogicRedeemer txInfo = Redeemer {getRedeemer = toBuiltinData glAct}
  where 
    glAct = AirdropGlobalLogicAction 
      { inputsIdxs = findAirdropValidatorInputIndices txInfo
      , outputIdx = 0  -- Assuming the first output is always the relevant one
      , numProcessed = fromIntegral $ length $ findAirdropValidatorInputIndices txInfo
      }

findAirdropValidatorInputIndices :: TxInfo -> [Integer]
findAirdropValidatorInputIndices txInfo = 
    map fromIntegral $ findIndices isAirdropValidatorInput (txInfoInputs txInfo)
  where
    isAirdropValidatorInput :: TxInInfo -> Bool
    isAirdropValidatorInput txIn = 
      txOutAddress (txInInfoResolved txIn) == mockAirdropValidatorAddress

renderPretty :: Pretty a => a -> String
renderPretty = renderString . layoutPretty defaultLayoutOptions . pretty

dumpDebug :: IO () 
dumpDebug = do 
  let config = airdropGlobalConfig (getPOSIXTime mockVestingStart) (getPOSIXTime mockVestingEnd)
      context = succeedsAirdropGlobalScriptContext config
      txInfo = scriptContextTxInfo context
  putStrLn "Inputs:"
  mapM_ (print . txInInfoResolved) (txInfoInputs txInfo)
  putStrLn "\nOutputs:"
  mapM_ print (txInfoOutputs txInfo)
  putStrLn "\nRedeemers"
  print (txInfoRedeemers txInfo)
  putStrLn "\n"
  
  putStrLn $ renderPretty txInfo
