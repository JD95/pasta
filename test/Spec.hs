{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import AST.Expr
import qualified AST.Expr as AST
import AST.LocTree
import Control.Applicative
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
import Runtime.Term
import Runtime.Types
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Options
import TypeCheck

main :: IO ()
main = do
  args <- getArgs
  withArgs ("--hide-successes" : args) $ defaultMain $ tests
  where
    tests = testGroup "compiler" [parsing, runtime, typeChecking]

    parsing =
      testGroup
        "parsing"
        [ testCase "() parses" unitParses,
          testCase "example parses" exampleParses,
          testCase "Annotations parse" annParses,
          testCase "annotations have highest parsing priority" annHasPriorityOverArrow
        ]

    runtime =
      testGroup
        "runtime"
        [testCase "() evals to ()" unitEvalsToUnit]

    typeChecking =
      testGroup
        "type checking"
        [ testCase "type merge is communative" typeMergeIsCommunative,
          testCase "unify is communative" unifyIsCommunative,
          testCase "errors raised while type checking are outputted" checkingErrorsGoThrough,
          testCase "inferred type of just () is ambiguous" cannotInferUnitTy,
          testCase "inferred type of foo is the provided type" infersSymbolTy,
          testCase "can infer type of arrow" infersArrTy,
          testCase "can infer type of symbols" infersSymbolTy,
          testCase "\"() : ()\" type checks" checkAnn,
          testCase "annotated lambdas type check" lambdasCheck
        ]

exampleParses = do
  void $ testParse "let id = \\x -> (\\y -> x) in (id y : t)"

unitParses = do
  result <- testParse "()"
  spine result @?= Prod []

annParses = do
  result <- testParse "() : ()"
  spine result @?= (Prod [] `Ann` Prod [])

annHasPriorityOverArrow = do
  result <- testParse "foo : () -> ()"
  spine result @?= (AST.Symbol "foo" `Ann` (unitE `Arr` unitE))

typeMergeIsCommunative = do
  void $
    withTyCheckM defaultTyCheckSt $ do
      u <- tyCell $ Filled unitF
      x <- tyCell $ Filled $ RtArrF u u
      i0 <- tyCell $ Empty
      y <- tyCell $ Filled $ RtArrF i0 i0
      RootInfo xCell _ _ _ _ <- rootInfo (unTyCell x)
      RootInfo yCell _ _ _ _ <- rootInfo (unTyCell y)
      let then_ a b = do
            inform b =<< (readRef $ value a)
            inform a =<< (readRef $ value b)
      let test = liftIO $ do
            xVal <- gatherTy x
            yVal <- gatherTy y
            xVal @?= yVal
      (xCell `then_` yCell *> test) <|> (yCell `then_` xCell *> test)

unifyIsCommunative = do
  void $
    withTyCheckM defaultTyCheckSt $ do
      u <- tyCell $ Filled unitF
      x <- tyCell $ Filled $ RtArrF u u
      i0 <- tyCell $ Empty
      y <- tyCell $ Filled $ RtArrF i0 i0
      unify x y
      xVal <- liftIO $ gatherTy x
      yVal <- liftIO $ gatherTy y
      liftIO $ xVal @?= RtArr unit unit
      liftIO $ yVal @?= RtArr unit unit

unitEvalsToUnit = do
  eval unit @?= unit

cannotInferUnitTy = do
  input <- testParse "()"
  typeCheck input defaultTyCheckSt noSetup
    `expectTyErrors` [AmbiguousTypes]

checkingErrorsGoThrough = do
  input <- testParse "foo : ()"
  let setup = do
        t <- tyCell $ Filled RtTyF
        void $ assuming "foo" $ Other $ TyExpr t unitF
  typeCheck input defaultTyCheckSt setup
    `expectTyErrors` [TypeMismatch]

infersArrTy = do
  input <- testParse "() -> ()"
  typeCheck input defaultTyCheckSt noSetup
    `expectTy` RtTy

infersSymbolTy = do
  input <- testParse "foo"
  let setup = do
        t <- tyCell $ Filled unitF
        void $ assuming "foo" $ Other $ TyExpr t unitF
  typeCheck input defaultTyCheckSt setup
    `expectTy` unit

lambdasCheck = do
  input <- testParse "(\\x -> x) : () -> ()"
  typeCheck input defaultTyCheckSt noSetup
    `expectTy` (RtArr unit unit)

checkAnn = do
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
    Right (LocTree _ _ (AnnotatedF actual _)) -> do
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
    Right (LocTree _ _ (AnnotatedF result _)) -> do
      assertFailure $
        "Type checking returned with:\n"
          <> show result
          <> "\nBut these errors were expected:\n"
          <> show expected
