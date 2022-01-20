{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import AST.Expr
import AST.LocTree
import Control.Exception (SomeException (..), try)
import Control.Monad
import Data.Functor.Foldable (cata)
import Data.IORef (IORef)
import Data.Maybe
import qualified Data.Vector as Vec
import Lexer
import Lib
import Parser
import Runtime
import Runtime.Dsl
import Runtime.Ref
import Runtime.Types (unit)
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
        [infersUnitTy]

exampleParses = testCase "example parses" $ do
  void $ testParse "let id = \\x -> (\\y -> x) in (id y : t)"

unitParses = testCase "() parses" $ do
  result <- testParse "()"
  spine result @?= Prod []

unitEvalsToUnit = testCase "() evals to ()" $ do
  eval unit @?= unit

infersUnitTy = testCase "inferred typed of () is ()" $ do
  input <- testParse "()"
  typeCheck input >>= \case
    Left _ -> error "wrong"
    Right tree -> do
      result <- extractTy tree
      result @?= (Prod [])

testLex input =
  case lexer "test" input of
    Right tokens -> pure tokens
    Left e -> assertFailure ("lex fail: " <> show e)

testParse input = do
  parse <$> (testLex input) >>= \case
    ([], report) -> assertFailure ("parse fail: " <> show report)
    (result : _, _) -> pure result
