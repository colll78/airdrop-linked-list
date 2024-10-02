{-# LANGUAGE TemplateHaskell #-}

module Types.AirdropGlobalLogic where 

import Plutarch.LedgerApi.V3
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude
import PlutusTx qualified


data AirdropGlobalLogicAction = AirdropGlobalLogicAction
  { inputsIdxs :: [Integer]
  , outputIdxs :: [Integer]
  }
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''AirdropGlobalLogicAction

data PAirdropGlobalLogicAction (s :: S)
  = PAirdropGlobalLogicAction
      ( Term
          s
          ( PDataRecord
              '[ "inputIdxs" ':= PBuiltinList (PAsData PInteger) 
               , "outputIdx" ':= (PAsData PInteger) 
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