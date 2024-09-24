{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE OverloadedStrings #-}

module Testing.MerklePatriciaForestry where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified Data.ByteString as BS
import Plutarch.Prelude
import MerkleTree.MerklePatriciaForestry
import MerkleTree.Helpers (pcombine)
import PriceDiscoveryEvent.Utils (pand'List)
import Testing.Eval (psucceeds, passert)
import Data.Word (Word8)
import Data.Char (toLower)
import PlutusLedgerApi.V2 (BuiltinByteString)
import PlutusTx.Prelude qualified as P
import qualified Data.ByteString.Char8 as BS8
import PlutusTx.Builtins.HasOpaque
import Data.Text qualified as T

proof_bitcoin_845999 :: ClosedTerm PProof
proof_bitcoin_845999 = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "bc13df27a19f8caf0bf922c900424025282a892ba8577095fd35256c9d553ca13a589f00f97a417d07903d138b92f25f879f9462994bf0e69b51fa19a67faef996c3f8196278c6ab196979911cc48b2d4a0d2a7aa5ef3f939eb056256d8efdfa0aa456963256af4fcb1ad43ef4e6323d1ca92c6d83ed4327904280228e1ba159"
      }
  , Branch
      { skip = 0
      , neighbors = toHexString "eb63f921bd3ac576f979eba32490f8c0988f468d3308c2ed5480aaf6ff27cf9a0e610d8c38c17236104b995eb83aa062181525dccd72a755772004cc2bf4faaf3ac3518525f4b5dec498c8034c566a3539e524c6a2cd5fc8f19c6559a32260513edca31960cd1f5cc6882b820ef57ca65d740734379781db22b479ae0e3bdef3"
      }
  , Branch
      { skip = 0
      , neighbors = toHexString "e7bbc4fc5e5875f6f5469e8a016fa99a872075360e64d623f8b8688e6b63fee5091a7260d2a4fe1ca489c48020772e6d334c63115743e7c390450a139c6bc63b219aff62993846b5522bc1b1fffb5b485fc58d952a8f171bb6a000062fbdcb0eaa5637413d82489f0492c663ad0bac0a2a83b32e1b14e3940017cf830d47378e"
      }
  , Branch
      { skip = 0
      , neighbors = toHexString "464f4d2211c7fe6e7e1b298be6cfa6fd35d562d3b37ce8b979df45fac9dbc5e0d4d93d0b14d7061351763cee1d878b8686c658cfca7ef69cfd58d50ffc3a467340c3abc4067220f82f2dffe455038da3138859bffdb3d34fd7e84305de2ddfc61630c97424469f6de887d42ca155069789fa1b843bdf26496d29222f33f8f6ae"
      }
  , Branch
      { skip = 0
      , neighbors = toHexString "2170e155c04db534b1f0e27bb7604907d26b046e51dd7ca59f56693e8033b16403f9ff21fe66b6071042d35dcbad83950ffb1e3a2ad6673f96d043f67d58e82040e0c17f6230c44b857ed04dccd8ff1b84819abf26fa9e1e86d61fb08c80b74c0000000000000000000000000000000000000000000000000000000000000000"
      }
  ]

test_verify_bitcoin_block_845999 :: Term s PBool
test_verify_bitcoin_block_845999 =
  let trie = pfrom_root # phexByteStr "225a4599b804ba53745538c83bfa699ecf8077201b61484c91171f5910a4a8f9"
      block_hash = phexByteStr "00000000000000000002d79d6d49c114e174c22b8d8432432ce45a05fd6a4d7b"
      block_body = phexByteStr "f48fcceeac43babbf53a90023be2799a9d7617098b76ff229440ccbd1fd1b4d4"
   in phas # trie # block_hash # block_body # proof_bitcoin_845999

toHexString :: String -> BuiltinByteString 
toHexString = 
  P.toBuiltin . BS.pack . f
  where
    f "" = []
    f [_] = error "UnevenLength"
    f (x : y : rest) = (hexDigitToWord8 x * 16 + hexDigitToWord8 y) : f rest

hexDigitToWord8 :: HasCallStack => Char -> Word8
hexDigitToWord8 = f . toLower
  where
    f :: Char -> Word8
    f '0' = 0
    f '1' = 1
    f '2' = 2
    f '3' = 3
    f '4' = 4
    f '5' = 5
    f '6' = 6
    f '7' = 7
    f '8' = 8
    f '9' = 9
    f 'a' = 10
    f 'b' = 11
    f 'c' = 12
    f 'd' = 13
    f 'e' = 14
    f 'f' = 15
    f c = error ("InvalidHexDigit " <> [c])

proof_bitcoin_845602 :: ClosedTerm PProof
proof_bitcoin_845602 = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "bc13df27a19f8caf0bf922c900424025282a892ba8577095fd35256c9d553ca120b8645121ebc9057f7b28fa4c0032b1f49e616dfb8dbd88e4bffd7c0844d29b011b1af0993ac88158342583053094590c66847acd7890c86f6de0fde0f7ae2479eafca17f9659f252fa13ee353c879373a65ca371093525cf359fae1704cf4a"
      }
  , Branch
      { skip = 0
      , neighbors = toHexString "255753863960985679b4e752d4b133322ff567d210ffbb10ee56e51177db057460b547fe42c6f44dfef8b3ecee35dfd4aa105d28b94778a3f1bb8211cf2679d7434b40848aebdd6565b59efdc781ffb5ca8a9f2b29f95a47d0bf01a09c38fa39359515ddb9d2d37a26bccb022968ef4c8e29a95c7c82edcbe561332ff79a51af"
      }
  , Branch
      { skip = 0
      , neighbors = toHexString "9d95e34e6f74b59d4ea69943d2759c01fe9f986ff0c03c9e25ab561b23a413b77792fa78d9fbcb98922a4eed2df0ed70a2852ae8dbac8cff54b9024f229e66629136cfa60a569c464503a8b7779cb4a632ae052521750212848d1cc0ebed406e1ba4876c4fd168988c8fe9e226ed283f4d5f17134e811c3b5322bc9c494a598b"
      }
  , Branch
      { skip = 0
      , neighbors = toHexString "b93c3b90e647f90beb9608aecf714e3fbafdb7f852cfebdbd8ff435df84a4116d10ccdbe4ea303efbf0f42f45d8dc4698c3890595be97e4b0f39001bde3f2ad95b8f6f450b1e85d00dacbd732b0c5bc3e8c92fc13d43028777decb669060558821db21a9b01ba5ddf6932708cd96d45d41a1a4211412a46fe41870968389ec96"
      }
  , Branch
      { skip = 0
      , neighbors = toHexString "f89f9d06b48ecc0e1ea2e6a43a9047e1ff02ecf9f79b357091ffc0a7104bbb260908746f8e61ecc60dfe26b8d03bcc2f1318a2a95fa895e4d1aadbb917f9f2936b900c75ffe49081c265df9c7c329b9036a0efb46d5bac595a1dcb7c200e7d590000000000000000000000000000000000000000000000000000000000000000"
      }
  ]

test_insert_bitcoin_block_845602 :: Term s PBool
test_insert_bitcoin_block_845602 = 
  let trie = pfrom_root # phexByteStr "225a4599b804ba53745538c83bfa699ecf8077201b61484c91171f5910a4a8f9"
      block_hash = phexByteStr "0000000000000000000261a131bf48cc5a19658ade8cfede99dc1c3933300d60"
      block_body = phexByteStr "26f711634eb26999169bb927f629870938bb4b6b4d1a078b44a6b4ec54f9e8df"
      expected_root = phexByteStr "507c03bc4a25fd1cac2b03592befa4225c5f3488022affa0ab059ca350de2353"
   in (pinsert # trie # block_hash # block_body # proof_bitcoin_845602) #== (pfrom_root # expected_root)


-- An example trie made from a list of fruits.
--
--   ╔═══════════════════════════════════════════════════════════════════╗
--   ║ #4acd78f345a686361df77541b2e0b533f53362e36620a1fdd3a13e0b61a3b078 ║
--   ╚═══════════════════════════════════════════════════════════════════╝
--    ┌─ 0 #520a7f805c5f
--    │  ├─ 389fd..[54 digits]..1abc #56408b9882f8 { mango[uid: 0] → 🥭 }
--    │  └─ 9d230..[54 digits]..9ecc #9ca49c0d73d5 { lemon[uid: 0] → 🍋 }
--    ├─ 16a4 #58c5e4a29601
--    │  ├─ 3a30b..[51 digits]..a968 #86410153344f { cherry[uid: 0] → 🍒 }
--    │  ├─ 8584c..[51 digits]..d4a5 #cda1c8929d05 { tomato[uid: 83468] → 🍅 }
--    │  └─ b7ce0..[51 digits]..f157 #472d5ccbcae8 { plum[uid: 15492] → 🤷 }
--    ├─ 245 #c9431d708d20
--    │  ├─ 4c787..[52 digits]..c20e #e38b422bd7d9 { pineapple[uid: 12577] → 🍍 }
--    │  ├─ a4f81..[52 digits]..90a3 #3e2491668264 { pomegranate[uid: 0] → 🤷 }
--    │  └─ e3fc8..[52 digits]..e7c3 #eda213c9a1ca { strawberry[uid: 2532] → 🍓 }
--    ├─ 3e #070a12b8b349
--    │  ├─ d002d..[53 digits]..f3ac #b40093af0024 { lime[uid: 0] → 🤷 }
--    │  └─ e659e..[53 digits]..b3b9 #242b464043b4 { banana[uid: 218] → 🍌 }
--    ├─ 4 #79519b8cdfbd
--    │  ├─ 07 #fdd60cf1b755
--    │  │  ├─ 6d8ab..[52 digits]..73ef #c538c893306a { guava[uid: 344] → 🤷 }
--    │  │  └─ c5847..[52 digits]..4a22 #785e20425cf9 { kiwi[uid: 0] → 🥝 }
--    │  └─ a522f..[54 digits]..20cd #e0b9d1f525e3 { kumquat[uid: 0] → 🤷 }
--    ├─ 5 #08434fd717ae
--    │  ├─ cddcd..[54 digits]..aa9e #8a1256a87426 { watermelon[uid: 0] → 🍉 }
--    │  └─ e #e26d8409cd76
--    │     ├─ 7ccfe..[53 digits]..4440 #c387ec2e54f6 { yuzu[uid: 0] → 🤷 }
--    │     └─ d71f9..[53 digits]..26d2 #cfcc9c732f50 { apple[uid: 58] → 🍎 }
--    ├─ 78666..[55 digits]..7292 #aeb3a9f2e198 { raspberry[uid: 0] → 🤷 }
--    ├─ 8af48..[55 digits]..04a8 #b27d20a5187a { tangerine[uid: 11] → 🍊 }
--    ├─ a #c2f2115774c1
--    │  ├─ 4b927..[54 digits]..3c69 #a6a35d200876 { peach[uid: 0] → 🍑 }
--    │  └─ f12 #8ee8d620e9d6
--    │     ├─ a1017..[51 digits]..50e7 #a241f4660aa4 { fig[uid: 68267] → 🤷 }
--    │     └─ ec412..[51 digits]..71fe #63c036b16617 { passionfruit[uid: 0] → 🤷 }
--    ├─ b #da0bdb30bf45
--    │  ├─ 67e71..[54 digits]..c48b #f39b1b5089f8 { grapefruit[uid: 0] → 🤷 }
--    │  └─ 88701..[54 digits]..949e #85acec96ac0f { blueberry[uid: 0] → 🫐 }
--    ├─ c #a22a7b4d767a
--    │  ├─ 5dc3c..[54 digits]..a3f3 #4c51531ac9d9 { cranberry[uid: 0] → 🤷 }
--    │  └─ 8cac1..[54 digits]..c3ca #8e27b4cf47de { orange[uid: 0] → 🍊 }
--    ├─ d #0a747d583e2e
--    │  ├─ b3047..[54 digits]..502a #54d9ea3b162d { coconut[uid: 0] → 🥥 }
--    │  └─ f779e..[54 digits]..678a #a82bdd8e07c2 { pear[uid: 0] → 🍐 }
--    ├─ e5993..[55 digits]..c9ec #da1771d107c8 { apricot[uid: 0] → 🤷 }
--    └─ f #117abf0e19fb
--       ├─ 63c88..[54 digits]..21ca #62bda6837164 { papaya[uid: 0] → 🤷 }
--       └─ b69c0..[54 digits]..2145 #c8e795f7b215 { grapes[uid: 0] → 🍇 }
--
ptrie :: ClosedTerm PMerklePatriciaForestry
ptrie = pfrom_root # phexByteStr "4acd78f345a686361df77541b2e0b533f53362e36620a1fdd3a13e0b61a3b078"


proof_kumquat :: ClosedTerm PProof
proof_kumquat = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c3490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d08434fd717aea47d156185d589f44a59fc2e0158eab7ff035083a2a66cd3e15b"
      }
  , Fork
      { skip = 0
      , neighbor = Neighbor
          { nibble = 0
          , prefix = toHexString "07"
          , root = toHexString "a1ffbc0e72342b41129e2d01d289809079b002e54b123860077d2d66added281"
          }
      }
  ]

kumquat :: ClosedTerm PByteString 
kumquat = pencodeUtf8 # pconstant "kumquat[uid: 0]"

kumquatVal :: ClosedTerm PByteString
kumquatVal = pencodeUtf8 # pconstant "🤷"

example_kumquat :: ClosedTerm PBool
example_kumquat =
  phas # ptrie # kumquat # kumquatVal # proof_kumquat

without_kumquat :: ClosedTerm PMerklePatriciaForestry
without_kumquat = pfrom_root # phexByteStr "4dd6d57ca8cb7ac8c3b219366754a392ba9e4e43b6b3ae59d89be3f878ba8fb6"

example_has :: ClosedTerm PBool
example_has = 
  pand'List
    [ phas # ptrie # papricot # (pencodeUtf8 # pconstant "🤷") # proof_apricot
    , phas # ptrie # pbanana # (pencodeUtf8 # pconstant "🍌") # proof_banana
    , phas # ptrie # pblueberry # (pencodeUtf8 # pconstant "🫐") # proof_blueberry
    , phas # ptrie # pcherry # (pencodeUtf8 # pconstant "🍒") # proof_cherry
    , phas # ptrie # pcoconut # (pencodeUtf8 # pconstant "🥥") # proof_coconut
    , phas # ptrie # pcranberry # (pencodeUtf8 # pconstant "🤷") # proof_cranberry
    , phas # ptrie # pgrapefruit # (pencodeUtf8 # pconstant "🤷") # proof_grapefruit
    , phas # ptrie # pgrapes # (pencodeUtf8 # pconstant "🍇") # proof_grapes
    , phas # ptrie # kumquat # (pencodeUtf8 # pconstant "🤷") # proof_kumquat
    , phas # ptrie # plemon # (pencodeUtf8 # pconstant "🍋") # proof_lemon
    , phas # ptrie # plime # (pencodeUtf8 # pconstant "🤷") # proof_lime
    , phas # ptrie # pmango # (pencodeUtf8 # pconstant "🥭") # proof_mango
    ] 

example_insert :: ClosedTerm PBool
example_insert = pand'List
  [ pinsert # without_apricot # papricot # (pencodeUtf8 # pconstant "🤷") # proof_apricot #== ptrie
  , pinsert # without_banana # pbanana # (pencodeUtf8 # pconstant "🍌") # proof_banana #== ptrie
  , pinsert # without_blueberry # pblueberry # (pencodeUtf8 # pconstant "🫐") # proof_blueberry #== ptrie
  , pinsert # without_cherry # pcherry # (pencodeUtf8 # pconstant "🍒") # proof_cherry #== ptrie
  , pinsert # without_coconut # pcoconut # (pencodeUtf8 # pconstant "🥥") # proof_coconut #== ptrie
  , pinsert # without_cranberry # pcranberry # (pencodeUtf8 # pconstant "🤷") # proof_cranberry #== ptrie
  , pinsert # without_grapefruit # pgrapefruit # (pencodeUtf8 # pconstant "🤷") # proof_grapefruit #== ptrie
  , pinsert # without_grapes # pgrapes # (pencodeUtf8 # pconstant "🍇") # proof_grapes #== ptrie
  , pinsert # without_kumquat # kumquat # (pencodeUtf8 # pconstant "🤷") # proof_kumquat #== ptrie
  , pinsert # without_lemon # plemon # (pencodeUtf8 # pconstant "🍋") # proof_lemon #== ptrie
  , pinsert # without_lime # plime # (pencodeUtf8 # pconstant "🤷") # proof_lime #== ptrie
  , pinsert # without_mango # pmango # (pencodeUtf8 # pconstant "🥭") # proof_mango #== ptrie
  , pinsert # without_raspberry # praspberry # (pencodeUtf8 # pconstant "🤷") # proof_raspberry #== ptrie
  ]

example_delete :: ClosedTerm PBool
example_delete = pand'List
  [ pdelete # ptrie # papricot # (pencodeUtf8 # pconstant "🤷") # proof_apricot #== without_apricot
  , pdelete # ptrie # pbanana # (pencodeUtf8 # pconstant "🍌") # proof_banana #== without_banana
  , pdelete # ptrie # pblueberry # (pencodeUtf8 # pconstant "🫐") # proof_blueberry #== without_blueberry
  , pdelete # ptrie # pcherry # (pencodeUtf8 # pconstant "🍒") # proof_cherry #== without_cherry
  , pdelete # ptrie # pcoconut # (pencodeUtf8 # pconstant "🥥") # proof_coconut #== without_coconut
  , pdelete # ptrie # pcranberry # (pencodeUtf8 # pconstant "🤷") # proof_cranberry #== without_cranberry
  , pdelete # ptrie # pgrapefruit # (pencodeUtf8 # pconstant "🤷") # proof_grapefruit #== without_grapefruit
  , pdelete # ptrie # pgrapes # (pencodeUtf8 # pconstant "🍇") # proof_grapes #== without_grapes
  , pdelete # ptrie # kumquat # (pencodeUtf8 # pconstant "🤷") # proof_kumquat #== without_kumquat
  , pdelete # ptrie # plemon # (pencodeUtf8 # pconstant "🍋") # proof_lemon #== without_lemon
  , pdelete # ptrie # plime # (pencodeUtf8 # pconstant "🤷") # proof_lime #== without_lime
  , pdelete # ptrie # pmango # (pencodeUtf8 # pconstant "🥭") # proof_mango #== without_mango
  , pdelete # ptrie # praspberry # (pencodeUtf8 # pconstant "🤷") # proof_raspberry #== without_raspberry
  ]

example_update :: ClosedTerm PBool 
example_update = 
  pupdate # ptrie # pbanana # proof_banana # (pencodeUtf8 # pconstant "🍌") # (pencodeUtf8 # pconstant "🍆") 
    #== updated_banana

tests :: TestTree
tests = testGroup "Merkle Patricia Forestry Tests"
  [ testCase "Verify Bitcoin Block 845999" $
      passert test_verify_bitcoin_block_845999
  , testCase "Insert Bitcoin Block 845602" $
      passert test_insert_bitcoin_block_845602
  , testCase "Has Kumquat" $
      passert example_kumquat
  , testCase "Example has" $
      passert example_has
  , testCase "Example Insert" $
      passert example_insert
  , testCase "Example Delete" $
      passert example_delete
  , testCase "Example Update" $ 
      passert example_update
  ]

-- Apricot
papricot :: ClosedTerm PByteString
papricot = pconstant "apricot[uid: 0]"

proof_apricot :: ClosedTerm PProof
proof_apricot = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbe280ada5ef30d55433934bbc73c89d550ee916f62822c34645e04bb66540c120f965c07fa815b86794e8703cee7e8f626c88d7da639258d2466aae67d5d041c5a117abf0e19fb78e0535891d82e5ece1310a1cf11674587dbba304c395769a988"
      }
  ]

without_apricot :: ClosedTerm PMerklePatriciaForestry
without_apricot = pfrom_root # phexByteStr "c08452d768160cd0fcdf5cad3d181cd36055eaf364d0eb7c49a01936bacf7b1f"

-- Raspberry
praspberry :: ClosedTerm PByteString
praspberry = pconstant "raspberry[uid: 0]"

proof_raspberry :: ClosedTerm PProof
proof_raspberry = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039cc9e7ff03faba170e98cd3c24338b95b1ce1b8a621d1016418f1494bbeb9e4a4a0000000000000000000000000000000000000000000000000000000000000000"
      }
  ]

without_raspberry :: ClosedTerm PMerklePatriciaForestry
without_raspberry = pfrom_root # phexByteStr "4c9d89603cb1a25361777b8ed7f7c80f71b1dea66603872feea2b34a83d34453"

-- Tangerine
ptangerine :: ClosedTerm PByteString
ptangerine = pconstant "tangerine[uid: 11]"

proof_tangerine :: ClosedTerm PProof
proof_tangerine = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbefa63eb4576001d8658219f928172eccb5448b4d7d62cd6d95228e13ebcbd5350c1e96bcc431893eef34e03989814375d439faa592edf75c9e5dc10b3c30766700000000000000000000000000000000000000000000000000000000000000000"
      }
  ]

without_tangerine :: ClosedTerm PMerklePatriciaForestry
without_tangerine = pfrom_root # phexByteStr "826a0c030ad675740b83a33653fd3fc32b1021233f709759292151abdcd37f8d"

-- Banana
pbanana :: ClosedTerm PByteString
pbanana = pencodeUtf8 # pconstant "banana[uid: 218]"

proof_banana :: ClosedTerm PProof
proof_banana = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5fcf22cbaac4ab605dd13dbde57080661b53d8a7e23534c733acf50125cf0e5bcac9431d708d20021f1fa3f4f03468b8de194398072a402e7877376d06f747575a"
      }
  , Leaf
      { skip = 1
      , key = toHexString "3ed002d6885ab5d92e1307fccd1d021c32ec429192aea10cb2fd688b92aef3ac"
      , value = toHexString "7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911"
      }
  ]

without_banana :: ClosedTerm PMerklePatriciaForestry
without_banana = pfrom_root # phexByteStr "557990b1257679f2b8e09c507f2582b0566579a2fc26d0d8a6b59a4a88ef16db"

updated_banana :: ClosedTerm PMerklePatriciaForestry
updated_banana = pfrom_root # phexByteStr "9057d02799a012a9d47fab6f9f5c43b4b2bf94584b339e3b4d3969fd95d55972"

-- Blueberry
pblueberry :: ClosedTerm PByteString
pblueberry = pconstant "blueberry[uid: 0]"

proof_blueberry :: ClosedTerm PProof
proof_blueberry = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbefa63eb4576001d8658219f928172eccb5448b4d7d62cd6d95228e13ebcbd5350be527bcfc7febe3c560057d97f4190bd24b537a322315f84daafab3ada562b50c2f2115774c117f184b58dba7a23d2c93968aa40387ceb0c9a9f53e4f594e881"
      }
  , Leaf
      { skip = 0
      , key = toHexString "b67e71b092e6a54576fa23b0eb48c5e5794a3fb5480983e48b40e453596cc48b"
      , value = toHexString "7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911"
      }
  ]

without_blueberry :: ClosedTerm PMerklePatriciaForestry
without_blueberry = pfrom_root # phexByteStr "e2025bb26dae9291d4eeb58817b5c7eb84ab2e47a27c994cc04369fffe8bc842"

-- Cherry
pcherry :: ClosedTerm PByteString
pcherry = pconstant "cherry[uid: 0]"

proof_cherry :: ClosedTerm PProof
proof_cherry = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f1508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d498417520a7f805c5f674e2deca5230b6942bbc71586dc94a783eebe1ed58c9a864e53"
      }
  , Branch
      { skip = 3
      , neighbors = toHexString "2549707d84ecc2fa100fd85bf15f2ec99da70d4b3a39588c1138331eb0e00d3e85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b10eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000"
      }
  ]

without_cherry :: ClosedTerm PMerklePatriciaForestry
without_cherry = pfrom_root # phexByteStr "968b14e351704108f00325985ab0cd81af8617bb131e31607b6bcd3f96d7c4c2"

-- Coconut
pcoconut :: ClosedTerm PByteString
pcoconut = pconstant "coconut[uid: 0]"

proof_coconut :: ClosedTerm PProof
proof_coconut = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbe280ada5ef30d55433934bbc73c89d550ee916f62822c34645e04bb66540c120f323def78732eace937391fc626efcd062552ebcf5e93f00352b86cb0f89daca0a22a7b4d767ada48673a4a9313a02a35ff47d2f55bcf10ae294127f590a4327c"
      }
  , Leaf
      { skip = 0
      , key = toHexString "df779e7f171b7299c2cede28bb898c1ee3456d98657b95e8082cd375704b678a"
      , value = toHexString "9e3d695f13a7292b8859d2ba0113e305825a8af8ba886d2ae73e73f2d35c6afe"
      }
  ]

without_coconut :: ClosedTerm PMerklePatriciaForestry
without_coconut = pfrom_root # phexByteStr "4888f3b72e475510bc0bb78c5f3706c0520a4294a41f8c05b5561776369d9d5d"

-- Cranberry
pcranberry :: ClosedTerm PByteString
pcranberry = pconstant "cranberry[uid: 0]"

proof_cranberry :: ClosedTerm PProof
proof_cranberry = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbe280ada5ef30d55433934bbc73c89d550ee916f62822c34645e04bb66540c120f323def78732eace937391fc626efcd062552ebcf5e93f00352b86cb0f89daca00a747d583e2e3db49524add1eea3063421fc04547e19c4e807810a537a63b379"
      }
  , Leaf
      { skip = 0
      , key = toHexString "c8cac1a325376bbc49936988b4c720d7806e99c878bc645ad90cebb98302c3ca"
      , value = toHexString "ccfd71674a4dca5f252690588b24bebffa36068206414b1575c0f7f7f8103839"
      }
  ]

without_cranberry :: ClosedTerm PMerklePatriciaForestry
without_cranberry = pfrom_root # phexByteStr "c80ac1ba6f8a6437562b25fe4a110f1c0013f26c7209f699df46493ce85e0081"

-- Grapefruit
pgrapefruit :: ClosedTerm PByteString
pgrapefruit = pconstant "grapefruit[uid: 0]"

proof_grapefruit :: ClosedTerm PProof
proof_grapefruit = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbefa63eb4576001d8658219f928172eccb5448b4d7d62cd6d95228e13ebcbd5350be527bcfc7febe3c560057d97f4190bd24b537a322315f84daafab3ada562b50c2f2115774c117f184b58dba7a23d2c93968aa40387ceb0c9a9f53e4f594e881"
      }
  , Leaf
      { skip = 0
      , key = toHexString "b88701c48c6abd03dfc5f4538bb585102ddc2e4640c55c8c3c9bb7e0093d949e"
      , value = toHexString "6d96ccb103b14005c17b3c17d45e0df0bab5dd1fb2276197a89ed1aeedaad7a0"
      }
  ]

without_grapefruit :: ClosedTerm PMerklePatriciaForestry
without_grapefruit = pfrom_root # phexByteStr "68125b51606cc784d3ed2010a2bc297776ce7442669a5072220f5e6911e5be84"

-- Grapes
pgrapes :: ClosedTerm PByteString
pgrapes = pconstant "grapes[uid: 0]"

proof_grapes :: ClosedTerm PProof
proof_grapes = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbe280ada5ef30d55433934bbc73c89d550ee916f62822c34645e04bb66540c120f965c07fa815b86794e8703cee7e8f626c88d7da639258d2466aae67d5d041c5ada1771d107c86c8e68da458063a47f9cdb63ddb9e922ab6ccb18d9e6d4b7aaf9"
      }
  , Leaf
      { skip = 0
      , key = toHexString "f63c88d1bc9695dfc39eaf90a11248964311383a95345e5b04d6d8f25d5121ca"
      , value = toHexString "7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911"
      }
  ]

without_grapes :: ClosedTerm PMerklePatriciaForestry
without_grapes = pfrom_root # phexByteStr "a5a405950c2aaf7da30abbfa969fdecccd4ed19077f751b1de641b2bfc2df957"

-- Lemon
plemon :: ClosedTerm PByteString
plemon = pconstant "lemon[uid: 0]"

proof_lemon :: ClosedTerm PProof
proof_lemon = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f1508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d49841758c5e4a29601399a5bd916e5f3b34c38e13253f4de2a3477114f1b2b8f9f2f4d"
      }
  , Leaf
      { skip = 0
      , key = toHexString "0389fd2d655e31dac50b00f3113aa327e36680e9df509d48eb255446d4891abc"
      , value = toHexString "001fb475e73fee4611a4350ae793d7dca387bcc1e199eabf498002a173378cc5"
      }
  ]

without_lemon :: ClosedTerm PMerklePatriciaForestry
without_lemon = pfrom_root # phexByteStr "6a7c7950e3718263c3f6d0b5cec7d7724c2394d62053692132c2ffebf8b8e4bd"

-- Lime
plime :: ClosedTerm PByteString
plime = pconstant "lime[uid: 0]"

proof_lime :: ClosedTerm PProof
proof_lime = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5fcf22cbaac4ab605dd13dbde57080661b53d8a7e23534c733acf50125cf0e5bcac9431d708d20021f1fa3f4f03468b8de194398072a402e7877376d06f747575a"
      }
  , Leaf
      { skip = 1
      , key = toHexString "3ee659e1fddc70f61cc65eb61478cd92a09fd7787ea4f913047469339f26b3b9"
      , value = toHexString "356a8eb7e12e71400ef0f2e305a89c643ec8cad60506ca9057201a5e36fb01ab"
      }
  ]

without_lime :: ClosedTerm PMerklePatriciaForestry
without_lime = pfrom_root # phexByteStr "cc11203c785e808fc0555562dd9fef4b9c161d2ed64ff16df47080325862f4a7"

-- Mango
pmango :: ClosedTerm PByteString
pmango = pconstant "mango[uid: 0]"

proof_mango :: ClosedTerm PProof
proof_mango = pcon $ PProof $ pconstant @(PBuiltinList PProofStep) $
  [ Branch
      { skip = 0
      , neighbors = toHexString "c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f1508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d49841758c5e4a29601399a5bd916e5f3b34c38e13253f4de2a3477114f1b2b8f9f2f4d"
      }
  , Leaf
      { skip = 0
      , key = toHexString "09d23032e6edc0522c00bc9b74edd3af226d1204a079640a367da94c84b69ecc"
      , value = toHexString "c29c35ad67a5a55558084e634ab0d98f7dd1f60070b9ce2a53f9f305fd9d9795"
      }
  ]

without_mango :: ClosedTerm PMerklePatriciaForestry
without_mango = pfrom_root # phexByteStr "c683f99382df709f322b957c3ff828ab10cb2b6a855458e4b3d23fbea83e7a0e"