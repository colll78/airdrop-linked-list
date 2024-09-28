module Scripts.NodeMP ( airdropNodeCS ) where

import Airdrop.Crypto (scriptHashV3)
import Plutarch.Script (Script(..))
import Plutarch.Internal (compile)
import Plutarch.Lift (pconstant)
import AirdropEvent.Mint.Standard (mkAirdropNodeMPW)
import Types.AirdropSet 
import Data.ByteString (ByteString)
import Plutarch 
import MerkleTree.MerklePatriciaForestry (MerklePatriciaForestry(..))
import PlutusLedgerApi.V1 (POSIXTime(..))
import PlutusLedgerApi.V3 (TxOutRef(..), CurrencySymbol(..))
import PlutusTx.Builtins (toBuiltin)
import Data.Either (fromRight)

defaultParams :: AirdropConfig 
defaultParams = 
  AirdropConfig 
    { initUTxO = TxOutRef "deadbeef" 0
    , totalVestingQty  = POSIXTime 100
    , claimRoot = MerklePatriciaForestry "74c61b3b5584c4434f03bc9acbe31d2d2186576e257f1fd85c997916d6df5715"
    , vestingPeriodEnd = 100  
    }

airdropNodeCS :: CurrencySymbol 
airdropNodeCS = CurrencySymbol $ toBuiltin $ scriptHashV3 $ fromRight (error "airdropNodeCS failed to compile") $ compile NoTracing $ mkAirdropNodeMPW # pconstant defaultParams

