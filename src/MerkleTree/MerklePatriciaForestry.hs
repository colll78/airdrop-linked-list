module MerkleTree.MerklePatriciaForestry where

import Plutarch.Prelude
import Plutarch.ByteString (PByteString)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl, pconstant)
import Plutarch.TryFrom (PTryFrom(..))
import Plutarch.DataRepr (PDataFields, PIsDataReprInstances(..), DerivePConstantViaData(..))
import Plutarch.Crypto (pblake2b_256, psliceByteString)
import MerkleTree.Helpers

-- Constants

pblake2b_256_digest_size :: Term s PInteger
pblake2b_256_digest_size = pconstant 32

-- Merkle Patricia Forestry

newtype PMerklePatriciaForestry (s :: S) = PMerklePatriciaForestry (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PMerklePatriciaForestry where type DPTStrat _ = PlutusTypeNewtype

pfrom_root :: Term s (PByteString :--> PMerklePatriciaForestry)
pfrom_root = phoistAcyclic $ plam $ \root ->
  pif (plengthBS # root #== pblake2b_256_digest_size)
    (pcon $ PMerklePatriciaForestry root)
    perror

pempty :: Term s PMerklePatriciaForestry
pempty = pcon $ PMerklePatriciaForestry pnull_hash

pis_empty :: Term s (PMerklePatriciaForestry :--> PBool)
pis_empty = phoistAcyclic $ plam $ \self ->
  pmatch self $ \(PMerklePatriciaForestry root) ->
    root #== pnull_hash

-- phas :: Term s (PMerklePatriciaForestry :--> PByteString :--> PByteString :--> PProof :--> PBool)
-- phas = phoistAcyclic $ plam $ \self key value proof ->
--   pmatch self $ \(PMerklePatriciaForestry root) ->
--     pincluding # key # value # proof #== root

data PProofStep (s :: S)
  = PBranch (Term s (PDataRecord '["skip" ':= PInteger, "neighbors" ':= PByteString]))
  | PFork (Term s (PDataRecord '["skip" ':= PInteger, "neighbor" ':= PNeighbor]))
  | PLeaf (Term s (PDataRecord '["skip" ':= PInteger, "key" ':= PByteString, "value" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PProofStep where type DPTStrat _ = PlutusTypeData

newtype PProof (s :: S) = PProof (Term s (PList PProofStep))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PProof where type DPTStrat _ = PlutusTypeNewtype

newtype PNeighbor (s :: S) = PNeighbor 
  (Term s (PDataRecord '["nibble" ':= PInteger, "prefix" ':= PByteString, "root" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PNeighbor where type DPTStrat _ = PlutusTypeNewtype

pdo_excluding :: Term s (PByteString :--> PInteger :--> PProof :--> PByteString)
pdo_excluding = phoistAcyclic $ plam $ \path cursor proof ->
  pmatch proof $ \case
    PProof PNil -> pnull_hash
    PProof (PCons pstep prest) -> pmatch pstep $ \case
      PBranch fields -> 
        plet (cursor + 1 + (pfield @"skip" # fields)) $ \nextCursor ->
        plet (pdo_excluding # path # nextCursor # (pcon $ PProof prest)) $ \root ->
        pdo_branch # path # cursor # nextCursor # root # (pfield @"neighbors" # fields)
      PFork fields -> 
        pif (pnull # prest)
          (plet (pfield @"neighbor" # fields) $ \neighbor ->
           plet (pconsBS # (pfield @"nibble" # neighbor) # (pfield @"prefix" # neighbor)) $ \prefix ->
           pcombine # prefix # (pfield @"root" # neighbor))
          (plet (cursor + 1 + (pfield @"skip" # fields)) $ \nextCursor ->
           plet (pdo_excluding # path # nextCursor # (pcon $ PProof prest)) $ \root ->
           pdo_fork # path # cursor # nextCursor # root # (pfield @"neighbor" # fields))
      PLeaf fields ->
        pif (pnull # prest)
          (pcombine # (psuffix # (pfield @"key" # fields) # cursor) # (pfield @"value" # fields))
          (plet (cursor + 1 + (pfield @"skip" # fields)) $ \nextCursor ->
           plet (pdo_excluding # path # nextCursor # (pcon $ PProof prest)) $ \root ->
           plet (pcon $ PNeighbor $ 
                  pdcons @"nibble" # (pnibble # (pfield @"key" # fields) # cursor) #
                  pdcons @"prefix" # (psuffix # (pfield @"key" # fields) # nextCursor) #
                  pdcons @"root" # (pfield @"value" # fields) # pdnil) $ \neighbor ->
           pdo_fork # path # cursor # nextCursor # root # neighbor)

pdo_branch :: Term s (PByteString :--> PInteger :--> PInteger :--> PByteString :--> PByteString :--> PByteString)
pdo_branch = phoistAcyclic $ plam $ \path cursor nextCursor root neighbors ->
  plet (pnibble # path # (nextCursor - 1)) $ \branch ->
  plet (pnibbles # path # cursor # (nextCursor - 1)) $ \prefix ->
  pcombine # prefix # 
    (pmerkle_16 # branch # root
      # (psliceBS # 0 # pblake2b_256_digest_size # neighbors)
      # (psliceBS # 32 # pblake2b_256_digest_size # neighbors)
      # (psliceBS # 64 # pblake2b_256_digest_size # neighbors)
      # (psliceBS # 96 # pblake2b_256_digest_size # neighbors))

pdo_fork :: Term s (PByteString :--> PInteger :--> PInteger :--> PByteString :--> PNeighbor :--> PByteString)
pdo_fork = phoistAcyclic $ plam $ \path cursor nextCursor root neighbor ->
  plet (pnibble # path # (nextCursor - 1)) $ \branch ->
  plet (pnibbles # path # cursor # (nextCursor - 1)) $ \prefix ->
  pif (branch #== (pfield @"nibble" # neighbor))
    perror
    (pcombine # prefix #
      (psparse_merkle_16 # branch # root # 
        (pfield @"nibble" # neighbor) # 
        (pcombine # (pfield @"prefix" # neighbor) # (pfield @"root" # neighbor))))
