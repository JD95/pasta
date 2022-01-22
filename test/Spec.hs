{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import AST.Expr
import AST.LocTree
import Control.Exception (SomeException (..), try)
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Foldable (cata)
import Data.IORef (IORef)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Vector as Vec
import Lexer
import Lib
import Parser
import Runtime
import Runtime.Dsl
import Runtime.Prop
import Runtime.Ref
import Runtime.Types
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Options
import TypeCheck

main :: IO ()
main = withArgs ["--hide-successes"] $ defaultMain $ tests
  where
    tests = testGroup "compiler" [parsing, runtime, typeChecking]

    parsing =
      testGroup
        "parsing"
        [unitParses, exampleParses]

    runtime =
      testGroup
        "runtime"
        [unitEvalsToUnit]

    typeChecking =
      testGroup
        "type checking"
        [infersUnitTy, infersSymbolTy]

exampleParses = testCase "example parses" $ do
  void $ testParse "let id = \\x -> (\\y -> x) in (id y : t)"

unitParses = testCase "() parses" $ do
  result <- testParse "()"
  spine result @?= Prod []

unitEvalsToUnit = testCase "() evals to ()" $ do
  eval unit @?= unit

infersUnitTy = testCase "inferred typed of () is ()" $ do
  input <- testParse "()"
  typeCheck input defaultTyCheckSt noSetup >>= \case
    Left _ -> error "wrong"
    Right tree -> do
      result <- extractTy tree
      result @?= (RtProd [])

infersSymbolTy = testCase "inferred type of foo is the provided type" $ do
  input <- testParse "foo"
  let setup = do
        tyCell <- TyCell <$> cell (Just $ RtProdF [])
        assuming "foo" $ TyExpr tyCell $ RtProdF []
  typeCheck input defaultTyCheckSt setup >>= \case
    Left _ -> error "wrong"
    Right tree -> do
      result <- extractTy tree
      result @?= (RtProd [])

testLex input =
  case lexer "test" input of
    Right tokens -> pure tokens
    Left e -> assertFailure ("lex fail: " <> show e)

testParse input = do
  parse <$> (testLex input) >>= \case
    ([], report) -> assertFailure ("parse fail: " <> show report)
    (result : _, _) -> pure result
