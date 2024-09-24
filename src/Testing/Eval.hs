{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Testing.Eval (evalT, evalWithArgsT, psucceeds, passert) where

import Cardano.Binary qualified as CBOR
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (
  first,
 )
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Text (
  Text,
  pack,
  unpack
 )
import Data.Text.Encoding qualified as Text
import Plutarch (
  Config (..),
  TracingMode (..),
  LogLevel (..), 
  compile,
  printScript,
 )
import Plutarch.Evaluate (
  evalScript,
  evalScriptHuge,
  applyArguments,
 )
import Plutarch.Prelude
import Plutarch.Script (Script, serialiseScript)
import PlutusLedgerApi.V2 (
  Data,
  ExBudget,
 )
import Test.Tasty.HUnit

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

evalT :: Config -> ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT cfg x = evalWithArgsT cfg x []

evalWithArgsT :: Config -> ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT cfg x args = do
  cmp <- compile cfg x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

writePlutusScript :: Config -> String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript cfg title filepath term = do
  case evalT cfg term of
    Left e -> putStrLn (show e)
    Right (script, _, _) -> do
      let
        scriptType = "PlutusScriptV2" :: String
        plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
        content = encodePretty plutusJson
      LBS.writeFile filepath content

writePlutusScriptTraceBind :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptTraceBind title filepath term =
  writePlutusScript (Tracing LogInfo DoTracingAndBinds) title filepath term

writePlutusScriptTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptTrace title filepath term =
  writePlutusScript (Tracing LogInfo DoTracing) title filepath term

writePlutusScriptNoTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptNoTrace title filepath term =
  writePlutusScript NoTracing title filepath term

comp :: ClosedTerm a -> Script
comp t = either (error . unpack) id $ compile (Tracing LogInfo DetTracing) t

-- | Asserts the term evaluates successfully without failing
psucceeds :: ClosedTerm a -> Assertion
psucceeds p =
  case evalScriptHuge $ comp p of
    (Left _, _, trc) -> assertFailure ("Term failed to evaluate" ++ show trc)
    (Right _, _, _) -> pure ()

pscriptShouldBe :: Script -> Script -> Assertion
pscriptShouldBe x y =
  assertEqual "pscriptShouldBe" (printScript x) (printScript y)

pshouldBe :: ClosedTerm a -> ClosedTerm b -> Assertion
pshouldBe x y = do
  p1 <- eval $ comp x
  p2 <- eval $ comp y
  pscriptShouldBe p1 p2
  where
    eval s = case evalScriptHuge s of
      (Left e, _, _) -> assertFailure $ "Script evaluation failed: " <> show e
      (Right x', _, _) -> pure x'

(#@?=) :: ClosedTerm a -> ClosedTerm b -> Assertion
(#@?=) = pshouldBe

-- | Asserts the term to be true
passert :: ClosedTerm a -> Assertion
passert p = p #@?= pconstant True
