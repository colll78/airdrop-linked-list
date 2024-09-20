
module MerkleTree.Helpers where
  
import Plutarch.ByteString (PByteString, pconsBS, psliceBS, plengthBS, pindexBS)
import Plutarch.Builtin (pblake2b_256)
import Plutarch.Integer (PInteger)
import Plutarch.Bool (PBool, (#==))
import Plutarch.Lift (pconstant)
import Plutarch.Prelude
    
-- Combine two ByteArrays using blake2b_256 hash
pcombine :: Term s (PByteString :--> PByteString :--> PByteString)
pcombine = phoistAcyclic $ plam $ \left right ->
  pblake2b_256 # (left <> right)

-- Calculate suffix of a path
psuffix :: Term s (PByteString :--> PInteger :--> PByteString)
psuffix = phoistAcyclic $ plam $ \path cursor -> 
  pif 
    (pmod # cursor # 2 #== 0)
    ((psliceBS # (pdiv # cursor # 2) # (plengthBS # path - pdiv # cursor # 2) # path) <> pconstant (BS.singleton 0xff))
    ((psliceBS # (pdiv # (cursor + 1) # 2) # (plengthBS # path - pdiv # (cursor + 1) # 2) # path) <> 
      (pconsBS # (pnibble # path # cursor) # pconstant (BS.singleton 0x00)))

-- Calculate nibbles for a branch node
pnibbles :: Term s (PByteString :--> PInteger :--> PInteger :--> PByteString)
pnibbles = phoistAcyclic $ plam $ \path start end ->
  precursive # plam (\self -> plam $ \s ->
    pif 
      (s #>= end)
      (pconstant BS.empty)
      (pconsBS # (pnibble # path # s) # (self # (s + 1)))
  ) # start

-- Calculate a single nibble
pnibble :: Term s (PByteString :--> PInteger :--> PInteger)
pnibble = phoistAcyclic $ plam $ \self index ->
  pif 
    (pmod # index # 2 #== 0)
    (pdiv # (pindexBS # self # (pdiv # index # 2)) # 16)
    (pmod # (pindexBS # self # (pdiv # index # 2)) # 16)

-- Helper functions

pdrop :: Term s (PInteger :--> PByteString :--> PByteString)
pdrop = phoistAcyclic $ plam $ \n bs -> 
  psliceBS # n # (plengthBS # bs - n) # bs 
