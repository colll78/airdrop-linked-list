{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}


module Types.AirdropGlobalLogic where 

import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude
import PlutusTx qualified


data AirdropGlobalLogicAction = AirdropGlobalLogicAction
  { inputsIdxs :: [Integer]
  , outputIdx :: Integer
  , numProcessed :: Integer 
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''AirdropGlobalLogicAction

data PAirdropGlobalLogicAction (s :: S)
  = PAirdropGlobalLogicAction
      ( Term
          s
          ( PDataRecord
              '[ "inputIdxs" ':= PBuiltinList (PAsData PInteger) 
               , "outputIdx" ':= PInteger
               , "numProcessed" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PAirdropGlobalLogicAction where
  type DPTStrat _ = PlutusTypeData


instance PUnsafeLiftDecl PAirdropGlobalLogicAction where
  type PLifted PAirdropGlobalLogicAction = AirdropGlobalLogicAction

deriving via
  (DerivePConstantViaData AirdropGlobalLogicAction PAirdropGlobalLogicAction)
  instance
    (PConstantDecl AirdropGlobalLogicAction)

instance PTryFrom PData (PAsData PAirdropGlobalLogicAction)
instance PTryFrom PData PAirdropGlobalLogicAction

data AirdropGlobalLogicConfig = AirdropGlobalLogicConfig
  { vestingPeriodStart :: Integer
  , vestingPeriodEnd :: Integer
  , timeBetweenInstallments :: Integer 
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''AirdropGlobalLogicConfig

data PAirdropGlobalLogicConfig (s :: S)
  = PAirdropGlobalLogicConfig
      ( Term
          s
          ( PDataRecord
              '[ "vestingPeriodStart" ':= PInteger
               , "vestingPeriodEnd" ':= PInteger
               , "timeBetweenInstallments" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PAirdropGlobalLogicConfig where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PAirdropGlobalLogicConfig where
  type PLifted PAirdropGlobalLogicConfig = AirdropGlobalLogicConfig

deriving via
  (DerivePConstantViaData AirdropGlobalLogicConfig PAirdropGlobalLogicConfig)
  instance
    (PConstantDecl AirdropGlobalLogicConfig)