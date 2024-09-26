{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types.LiquiditySet where

import Plutarch.LedgerApi.V3
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Monadic qualified as P
import GHC.Generics (Generic)
import Plutarch.Prelude
import PlutusLedgerApi.V2 (BuiltinByteString, PubKeyHash, TokenName, CurrencySymbol, Address)
import PlutusTx qualified
import Types.Classes 
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Builtin (PDataNewtype (PDataNewtype), pforgetData)
import Plutarch.Bool (pand')
import MerkleTree.MerklePatriciaForestry (PMerklePatriciaForestry (..), PProof(..))
import PriceDiscoveryEvent.Utils (pcond, pand'List)
import Plutarch.Internal.PlutusType (pcon', pmatch')

data NodeKey = Key BuiltinByteString | Empty
  deriving stock (Show, Eq, Ord, Generic)
PlutusTx.unstableMakeIsData ''NodeKey

data PNodeKey (s :: S)
  = PKey (Term s (PDataRecord '["_0" ':= PByteString]))
  | PEmpty (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

deriving via
  (DerivePConstantViaData NodeKey PNodeKey)
  instance
    PConstantDecl NodeKey

instance PUnsafeLiftDecl PNodeKey where
  type PLifted PNodeKey = NodeKey

deriving anyclass instance
  PTryFrom PData PNodeKey

instance DerivePlutusType PNodeKey where type DPTStrat _ = PlutusTypeData

data PNodeKeyState (s :: S)
  = PKeyScott (Term s PByteString)
  | PEmptyScott
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PNodeKeyState where type DPTStrat _ = PlutusTypeScott

instance ScottConvertible PNodeKey where
  type ScottOf PNodeKey = PNodeKeyState
  toScott nodeKey = pmatch nodeKey $ \case
    PKey kname -> pcon (PKeyScott (pfield @"_0" # kname))
    PEmpty _ -> pcon PEmptyScott
  fromScott nodeKeyScott = pmatch nodeKeyScott $ \case
    PKeyScott bs -> pcon (PKey (pdcons # pdata bs # pdnil))
    PEmptyScott -> pcon (PEmpty pdnil)


data PProxyTokenHolderDatum (s :: S)
  = PProxyTokenHolderDatum
      ( Term
          s
          ( PDataRecord
              '[ "totalCommitted" ':= PInteger 
               , "returnAddress" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PProxyTokenHolderDatum where
  type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PProxyTokenHolderDatum
  

data LNodeAction
  = LLinkedListAct
  | PartialUnlock
  | FullUnlock
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''LNodeAction

data PLNodeAction (s :: S)
  = PLLinkedListAct (Term s (PDataRecord '[]))
  | PPartialUnlock (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PLNodeAction where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PLNodeAction where
  type PLifted PLNodeAction = LNodeAction

deriving via
  (DerivePConstantViaData LNodeAction PLNodeAction)
  instance
    (PConstantDecl LNodeAction)

instance PTryFrom PData (PAsData PLNodeAction)
instance PTryFrom PData PLNodeAction

data PClaimValidatorConfig (s :: S)
  = PClaimValidatorConfig
      ( Term
          s
          ( PDataRecord
              '[ "discoveryDeadline" ':= PPosixTime
               , "commitCred" ':= PStakingCredential
               , "rewardCred" ':= PStakingCredential
               , "vestingPeriodStart" ':= PInteger 
               , "vestingPeriodEnd" ':= PInteger 
               ]
          )
      )
  deriving stock (Generic) 
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PClaimValidatorConfig where type DPTStrat _ = PlutusTypeData

data PAirdropConfig (s :: S)
  = PAirdropConfig
      ( Term
          s
          ( PDataRecord
              '[ "initUTxO" ':= PTxOutRef
               , "claimDeadline" ':= PPosixTime
               , "claimRoot" ':= PMerklePatriciaForestry
               , "vestingPeriodEnd" ':= PInteger 
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PAirdropConfig where type DPTStrat _ = PlutusTypeData

data VestingDatum = VestingDatum
  { beneficiary :: Address
  , vestingTN :: TokenName 
  , vestingCS :: CurrencySymbol
  , totalVestingQty :: Integer
  , vestingPeriodStart :: Integer
  , vestingPeriodEnd :: Integer
  , firstUnlockPossibleAfter :: Integer
  , totalInstallments :: Integer
  }
  deriving stock (Show, Eq, Generic)

PlutusTx.makeLift ''VestingDatum
PlutusTx.makeIsDataIndexed ''VestingDatum [('VestingDatum, 0)]

data PVestingDatum (s :: S)
  = PVestingDatum
      ( Term
          s
          ( PDataRecord
              '[ "beneficiary" ':= PAddress
               , "totalVestingQty" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PVestingDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PVestingDatum

data AirdropSetNode = MkAirdropSetNode
  { key :: NodeKey
  , next :: NodeKey
  , committed :: VestingDatum 
  }
  deriving stock (Show, Eq, Generic)
PlutusTx.unstableMakeIsData ''AirdropSetNode

data PAirdropSetNodeState (s :: S) = PAirdropSetNodeState
  { key :: Term s PNodeKeyState
  , next :: Term s PNodeKeyState
  , committed :: Term s PVestingDatum
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PAirdropSetNodeState where type DPTStrat _ = PlutusTypeScott

data PAirdropSetNode (s :: S)
  = PAirdropSetNode
      ( Term
          s
          ( PDataRecord
              '[ "key" ':= PNodeKey
               , "next" ':= PNodeKey
               , "extraData" ':= PData  
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PAirdropSetNode where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PAirdropSetNode

deriving anyclass instance
  PTryFrom PData (PAsData PAirdropSetNode)

instance PUnsafeLiftDecl PAirdropSetNode where
  type PLifted PAirdropSetNode = AirdropSetNode

deriving via
  (DerivePConstantViaData AirdropSetNode PAirdropSetNode)
  instance
    PConstantDecl AirdropSetNode


instance ScottConvertible PInteger where
  type ScottOf PInteger = PInteger
  toScott i = i
  fromScott i = i

-- instance ScottConvertible PAirdropSetNode where
--   type ScottOf PAirdropSetNode = PAirdropSetNodeState
--   toScott discSetNode' = pmatch discSetNode' $ \(PAirdropSetNode discSetNode) -> pletFields @'["key", "next", "commitment"] discSetNode $ \discSetNodeF -> 
--     pcon (PAirdropSetNodeState {key = toScott discSetNodeF.key, next = toScott discSetNodeF.next, committed = discSetNodeF.commitment})
--   fromScott discSetNode = pmatch discSetNode $
--       \( PAirdropSetNodeState
--           { key
--           , next
--           , committed 
--           }
--         ) -> 
--           (pcon (PAirdropSetNode 
--             (pdcons @"key" # pdata (fromScott key) 
--               #$ (pdcons @"next" # pdata (fromScott next))
--               #$ (pdcons @"commitment" # pdata committed)
--               #$ pdnil)))

data PSeparatorConfig (s :: S)
  = PSeparatorConfig
      ( Term
          s
          ( PDataRecord
              '[ "signer" ':= PPubKeyHash
               , "cutOff" ':= PPosixTime
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PSeparatorConfig where type DPTStrat _ = PlutusTypeData

mkNode :: Term s (PNodeKey :--> PNodeKey :--> PVestingDatum :--> PAirdropSetNode)
mkNode = phoistAcyclic $
  plam $ \key next vestingDatum ->
    pcon $
      PAirdropSetNode $
        pdcons @"key" # pdata key
          #$ pdcons @"next" # pdata next
          #$ pdcons @"extraData" # (punsafeCoerce $ pdata vestingDatum)
          #$ pdnil

data AirdropNodeAction
  = Init
  | Deinit
  | -- | first arg is the key to insert, second arg is the covering node
    Insert BuiltinByteString 
  | -- | first arg is the key to remove, second arg is the covering node
    Remove BuiltinByteString
  deriving stock (Show, Eq, Generic)
PlutusTx.unstableMakeIsData ''AirdropNodeAction

data PAirdropNodeAction (s :: S)
  = PLInit (Term s (PDataRecord '[]))
  | PLDeinit (Term s (PDataRecord '[]))
  | PLInsert (Term s (PDataRecord '["keyToInsert" ':= PByteString, "claimData" ':= PSignatureType]))
  | PLRemove (Term s (PDataRecord '["keyToRemove" ':= PByteString, "coveringNode" ':= PAirdropSetNode]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PAirdropNodeAction where type DPTStrat _ = PlutusTypeData

-- deriving anyclass instance
--   PTryFrom PData (PAsData PAirdropNodeAction)

-- instance PUnsafeLiftDecl PAirdropNodeAction where
--   type PLifted PAirdropNodeAction = AirdropNodeAction
-- deriving via
--   (DerivePConstantViaData AirdropNodeAction PAirdropNodeAction)
--   instance
--     PConstantDecl AirdropNodeAction

data PSignatureType (s :: S)
  = Ed25519Signature (Term s (PDataRecord '["signature" ':= PByteString, "network" ':= PNetwork, "amount" ':= PInteger, "claimAddr" ':= PAddress, "proof" ':= PProof]))
  | SchnorrSignature (Term s (PDataRecord '["signature" ':= PByteString, "network" ':= PNetwork, "amount" ':= PInteger, "claimAddr" ':= PAddress, "proof" ':= PProof]))
  | EcdsaSecpSignature (Term s (PDataRecord '["signature" ':= PByteString, "network" ':= PNetwork, "amount" ':= PInteger, "claimAddr" ':= PAddress, "proof" ':= PProof]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PSignatureType where type DPTStrat _ = PlutusTypeData

data PNetwork (s :: S) = PBitcoin | PEthereum | PCardano | PSolana 

{- |
  PNetwork is encoded as an Enum, using values of PInteger
  internally.
-}
instance PlutusType PNetwork where
  type PInner PNetwork = PInteger

  pcon' PBitcoin = 0
  pcon' PEthereum = 1
  pcon' PCardano = 2
  pcon' PSolana = 3

  pmatch' x f =
    pcond 
      [ ((x #== 0), (f PBitcoin))
      , ((x #== 1), (f PEthereum))
      , ((x #== 2), (f PCardano))
      , ((x #== 3), (f PSolana))
      ]
      perror 

-----------------------------------------------
-- Helpers:

mkBSNode :: ClosedTerm (PByteString :--> PByteString :--> PVestingDatum :--> PAirdropSetNode)
mkBSNode = phoistAcyclic $
  plam $ \key' next' vestingDatum ->
    let key = pcon $ PKey $ pdcons @"_0" # pdata key' #$ pdnil
        next = pcon $ PKey $ pdcons @"_0" # pdata next' #$ pdnil
     in mkNode # key # next # vestingDatum 

-- | Checks that the node is the empty head node and the datum is empty
isEmptySet :: ClosedTerm (PAsData PAirdropSetNode :--> PBool)
isEmptySet = phoistAcyclic $
  plam $ \head -> P.do
    keys <- pletFields @'["key", "next", "extraData"] head
    isNothing # pfromData keys.key #&& isNothing # pfromData keys.next #&& keys.extraData #== (pforgetData $ pconstantData ()) 

-- | Checks that a PubKeyHash does belong to the first Node in the set.
isFirstNode :: ClosedTerm (PByteString :--> PAirdropSetNode :--> PBool)
isFirstNode = phoistAcyclic $
  plam $ \key node -> P.do
    keys <- pletFields @'["key", "next"] node
    pmatch (keys.next) $ \case
      PKey n ->
        key #== pfromData (pfield @"_0" # n) #&& isNothing # pfromData keys.key
      _ -> pcon PFalse

-- | Checks that a PubkeyHash does belong to the last Node in a set.
isLastNode :: ClosedTerm (PByteString :--> PAirdropSetNode :--> PBool)
isLastNode = phoistAcyclic $
  plam $ \key node -> P.do
    keys <- pletFields @'["key", "next"] node
    pmatch (keys.key) $ \case
      PKey ((pfield @"_0" #) -> n) ->
        key #== pfromData n #&& isNothing # pfromData keys.next
      _ -> pcon PFalse

-- | Checks that node key is absent.
isNothing :: Term s (PNodeKey :--> PBool)
isNothing = phoistAcyclic $
  plam $ \md -> pmatch md $ \case
    PKey _ -> pcon PFalse
    PEmpty _ -> pcon PTrue

pisInsertedOnNode :: ClosedTerm (PNodeKey :--> PNodeKey :--> PData :--> PAsData PAirdropSetNode :--> PBool)
pisInsertedOnNode = phoistAcyclic $
  plam $ \insertedKey coveringKey expectedExtraData outputNode -> P.do
    outputNodeDatum <- pletFields @'["key", "next", "extraData"] outputNode
    pand'List 
      [ (outputNodeDatum.key #== coveringKey)
      , (outputNodeDatum.next #== insertedKey)
      , (outputNodeDatum.extraData #== expectedExtraData)
      ]

pisInsertedNode :: ClosedTerm (PNodeKey :--> PNodeKey :--> PData :--> PAsData PAirdropSetNode :--> PBool)
pisInsertedNode = phoistAcyclic $ 
  plam $ \insertedKey coveringNext expectedExtraData outputNode ->
    pletFields @'["key", "next", "extraData"] outputNode $ \outputNodeDatumF ->
      pand'List 
        [ (outputNodeDatumF.key #== insertedKey)
        , (outputNodeDatumF.next #== coveringNext)
        , (outputNodeDatumF.extraData #== expectedExtraData)
        ]


-- | Extracts the next node key
getNextPK :: ClosedTerm (PAsData PAirdropSetNode :--> PMaybe PPubKeyHash)
getNextPK = phoistAcyclic $
  plam $ \node ->
    let nextNodeKey = pfromData $ pfield @"next" # node
     in pmatch nextNodeKey $ \case
          PEmpty _ -> pcon PNothing
          PKey ((pfield @"_0" #) -> n) -> pcon $ PJust $ pcon $ PPubKeyHash $ pcon $ PDataNewtype n

-- | Extracts the node key
getCurrentPK :: ClosedTerm (PAsData PAirdropSetNode :--> PMaybe PPubKeyHash)
getCurrentPK = phoistAcyclic $
  plam $ \node ->
    let nodeKey = pfromData $ pfield @"key" # node
     in pmatch nodeKey $ \case
          PEmpty _ -> pcon PNothing
          PKey ((pfield @"_0" #) -> n) -> 
            pcon $ PJust $ pcon $ PPubKeyHash $ pcon $ PDataNewtype n

{- | Checks whether @SetNode@ key is less than next node key.
  Any valid sequence of nodes MUST follow this property.
-}
validNode :: ClosedTerm (PAsData PAirdropSetNode :--> PBool)
validNode = phoistAcyclic $
  plam $ \node -> P.do
    nodeDatum <- pletFields @'["key", "next"] node
    pmatch (nodeDatum.key) $ \case
      PEmpty _ -> pcon PTrue
      PKey ((pfield @"_0" #) -> key) -> pmatch (nodeDatum.next) $ \case
        PEmpty _ -> pcon PTrue
        PKey ((pfield @"_0" #) -> next) ->
          pfromData key #< pfromData next -- nodes ordered incrementally

coversLiquidityKey :: ClosedTerm (PAsData PAirdropSetNode :--> PByteString :--> PBool)
coversLiquidityKey = phoistAcyclic $
  plam $ \datum keyToCover -> P.do
    nodeDatum <- pletFields @'["key", "next"] datum
    let moreThanKey = pmatch (nodeDatum.key) $ \case
          PEmpty _ -> pcon PTrue
          PKey (pfromData . (pfield @"_0" #) -> key) -> key #< keyToCover
        lessThanNext = pmatch (nodeDatum.next) $ \case
          PEmpty _ -> pcon PTrue
          PKey (pfromData . (pfield @"_0" #) -> next) -> keyToCover #< next
    moreThanKey #&& lessThanNext