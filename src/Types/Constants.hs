{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types.Constants where

import Plutarch
import Plutarch.LedgerApi.V1 (PTokenName (..), PCurrencySymbol(..), PPubKeyHash(..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import PlutusLedgerApi.V1 (TokenName, CurrencySymbol)
import PriceDiscoveryEvent.Utils (pnonew, passert, pisPrefixOf)
import Plutarch.Builtin (PDataNewtype(..))
import MerkleTree.MerklePatriciaForestry
import Plutarch.ByteString

claimRoot :: ClosedTerm PMerklePatriciaForestry
claimRoot = pfrom_root # phexByteStr "74c61b3b5584c4434f03bc9acbe31d2d2186576e257f1fd85c997916d6df5715"

totalVestingInstallments :: ClosedTerm PInteger 
totalVestingInstallments = 4

airdropOperator :: ClosedTerm (PAsData PPubKeyHash)
airdropOperator = pconstantData "deadbeef"

claimTokenTN :: Term s PTokenName
claimTokenTN =
  let tn :: TokenName
      tn = "FooToken"
   in pconstant tn

claimTokenCS :: Term s PCurrencySymbol
claimTokenCS =
  let cs :: CurrencySymbol
      cs = "deadbeef"
   in pconstant cs

commitFoldTN :: Term s PTokenName
commitFoldTN =
  let tn :: TokenName
      tn = "CFold"
   in pconstant tn

rewardFoldTN :: Term s PTokenName
rewardFoldTN =
  let tn :: TokenName
      tn = "RFold"
   in pconstant tn

poriginNodeTN :: Term s PTokenName
poriginNodeTN =
  let tn :: TokenName
      tn = "FSN"
   in pconstant tn

psetNodePrefix :: ClosedTerm PByteString
psetNodePrefix = pconstant "FSN"

pnodeKeyTN :: ClosedTerm (PByteString :--> PTokenName)
pnodeKeyTN = phoistAcyclic $
  plam $
    \nodeKey -> pcon $ PTokenName $ pcon $ PDataNewtype $ pdata $ psetNodePrefix <> nodeKey

pparseNodeKey :: ClosedTerm (PTokenName :--> PMaybe PByteString)
pparseNodeKey = phoistAcyclic $
  plam $ \(pnonew -> tn) -> P.do
    let prefixLength = 3
        tnLength = plengthBS # tn
        key = psliceBS # prefixLength # (tnLength - prefixLength) # tn
    passert "incorrect node prefix" $ pisPrefixOf # psetNodePrefix # tn
    pif (prefixLength #< tnLength) (pcon $ PJust key) (pcon PNothing)

foldingFee :: Term s PInteger
foldingFee = pconstant 1_000_000

minAda :: Term s PInteger
minAda = pconstant 2_000_000

-- 3 Ada for min UTxO Ada for the node UTxO 
-- 1 Ada for the collection fold fee 
-- 1 Ada for the reward fold fee 
nodeDepositAda :: Term s PInteger 
nodeDepositAda = pconstant 5_000_000

minAdaToCommit :: Term s PInteger 
minAdaToCommit = pconstant 3_000_000