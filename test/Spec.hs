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
        [unitParses, exampleParses, annParses]

    runtime =
      testGroup
        "runtime"
        [unitEvalsToUnit]

    typeChecking =
      testGroup
        "type checking"
        [checkingErrorsGoThrough, cannotInferUnitTy, infersSymbolTy, checkAnn]

exampleParses = testCase "example parses" $ do
  void $ testParse "let id = \\x -> (\\y -> x) in (id y : t)"

unitParses = testCase "() parses" $ do
  result <- testParse "()"
  spine result @?= Prod []

annParses = testCase "Annotations parse" $ do
  result <- testParse "() : ()"
  spine result @?= (Prod [] `Ann` Prod [])

unitEvalsToUnit = testCase "() evals to ()" $ do
  eval unit @?= unit

cannotInferUnitTy = testCase "inferred typed of () is ambiguous" $ do
  input <- testParse "()"
  typeCheck input defaultTyCheckSt noSetup
    `expectTyErrors` [AmbiguousTypes]

checkingErrorsGoThrough = testCase "errors raised while type checking are outputted" $ do
  input <- testParse "foo : ()"
  let setup = do
        t <- tyCell $ Filled RtTyF
        void $ assuming "foo" $ Other $ TyExpr t unitF
  typeCheck input defaultTyCheckSt setup
    `expectTyErrors` [TypeMismatch]

infersSymbolTy = testCase "inferred type of foo is the provided type" $ do
  input <- testParse "foo"
  let setup = do
        t <- tyCell $ Filled unitF
        void $ assuming "foo" $ Other $ TyExpr t unitF
  typeCheck input defaultTyCheckSt setup
    `expectTy` unit

checkAnn = testCase "\"() : ()\" type checks" $ do
  input <- testParse "() : ()"
  typeCheck input defaultTyCheckSt noSetup
    `expectTy` unit

testLex input =
  case lexer "test" input of
    Right tokens -> pure tokens
    Left e -> assertFailure ("lex fail: " <> show e)

testParse input = do
  parse <$> (testLex input) >>= \case
    ([], report) -> assertFailure ("parse fail: " <> show report)
    (result : _, _) -> pure result

expectTy check expected =
  check >>= \case
    Left actual ->
      assertFailure $
        "Type checking returned these errors:\n"
          <> show actual
          <> "\nBut it was supposed to succeed with:\n"
          <> show expected
    Right tree -> do
      actual <- extractTy tree
      actual @?= expected

expectTyErrors check expected =
  check >>= \case
    Left actual ->
      if actual == expected
        then pure ()
        else
          assertFailure $
            "Type checking returned these errors:\n"
              <> show actual
              <> "\nBut these errors were expected:\n"
              <> show expected
    Right tree -> do
      result <- extractTy tree
      assertFailure $
        "Type checking returned with:\n"
          <> show result
          <> "\nBut these errors were expected:\n"
          <> show expected
