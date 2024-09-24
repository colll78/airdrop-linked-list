{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# HLINT ignore "Eta reduce" #-}
module Main (main) where


import Data.Default (Default (def))
import Plutarch
import Plutarch.LedgerApi.V1 
import Plutarch.Builtin
import Plutarch.Lift
import Plutarch.List
import Plutarch.Unsafe
import Plutarch.Script
import Testing.Eval (evalT, evalWithArgsT)
import Plutarch.Bool
import Plutarch.Prelude 
import PlutusTx qualified 
import PlutusTx.Prelude qualified as PlutusTx

integerList :: Term s (PBuiltinList PInteger)
integerList = pconstant @(PBuiltinList PInteger) [1,2,3,4]

main :: IO ()
main = do
  putStrLn "elemAt: naive"
  case evalT NoTracing
    (  ptail # integerList 
    ) of
    Right (result, budget, trc) -> print (unScript result) >> print trc >> print budget
    Left err -> print err

  -- putStrLn "elemAt: naive"
  -- case evalT NoTracing
  --   (  ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail #$ ptail # integerList
  --   ) of
  --   Right (result, budget, trc) -> print (unScript result) >> print trc >> print budget
  --   Left err -> print err


