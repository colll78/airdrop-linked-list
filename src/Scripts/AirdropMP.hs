module Scripts.AirdropMP ( airdropTokenCS, airdropScriptBytes ) where

import Airdrop.Crypto (scriptHashV3)
import AirdropEvent.Mint.AirdropToken (mkAirdropTokenMP)
import Plutarch 
import Plutarch.Script (serialiseScript)
import PlutusLedgerApi.V3 (CurrencySymbol(..))
import PlutusTx.Builtins (toBuiltin)
import Data.Either (fromRight)
import Data.ByteString.Short qualified as SBS 

airdropScriptBytes :: SBS.ShortByteString 
airdropScriptBytes = serialiseScript $ fromRight (error "airdropTokenMP failed to compile") $ compile NoTracing $ mkAirdropTokenMP

airdropTokenCS :: CurrencySymbol 
airdropTokenCS = CurrencySymbol $ toBuiltin $ scriptHashV3 $ fromRight (error "airdropTokenMP failed to compile") $ compile NoTracing $ mkAirdropTokenMP