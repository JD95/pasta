{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import AST.Expr
import qualified AST.Expr as AST
import AST.LocTree
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import Lexer
import Lib
import Runtime.Prop
import Runtime.Term
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit

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
        [ testCase "errors are properly filtered" errorFilter,
          testCase "type merge is communative" typeMergeIsCommunative,
          testCase "unify is communative" unifyIsCommunative,
          testCase "errors raised while type checking are outputted" checkingErrorsGoThrough,
          testCase "inferred type of foo is the provided type" infersSymbolTy,
          testCase "can infer type of arrow" infersArrTy,
          testCase "can infer type of symbols" infersSymbolTy,
          testCase "\"() : ()\" type checks" checkAnn,
          testCase "annotated lambdas type check" lambdasCheck,
          testCase "non-dependent function application checks" checkAppNonDep
        ]

exampleParses :: IO ()
exampleParses = do
  void $ testParse "let id = \\x -> (\\y -> x) in (id y : t)"

unitParses :: IO ()
unitParses = do
  result <- testParse "()"
  spine result @?= Prod []

annParses :: IO ()
annParses = do
  result <- testParse "() : ()"
  spine result @?= (Prod [] `Ann` Prod [])

annHasPriorityOverArrow :: IO ()
annHasPriorityOverArrow = do
  result <- testParse "foo : () -> ()"
  spine result @?= (AST.Symbol "foo" `Ann` (unitE `Arr` unitE))

typeMergeIsCommunative :: IO ()
typeMergeIsCommunative = do
  void $
    withTyCheckM defaultTyCheckSt $ do
      u <- tyTerm $ Filled unitF
      x <- tyTerm $ Filled $ RtArrF u u
      i0 <- tyTerm $ Empty
      y <- tyTerm $ Filled $ RtArrF i0 i0
      RootInfo xCell _ _ _ _ <- rootInfo (unTyTerm x)
      RootInfo yCell _ _ _ _ <- rootInfo (unTyTerm y)
      let then_ a b = do
            inform b =<< (readRef $ value a)
            inform a =<< (readRef $ value b)
      let test = liftIO $ do
            xVal <- gatherTy x
            yVal <- gatherTy y
            xVal @?= yVal
      (xCell `then_` yCell *> test) <|> (yCell `then_` xCell *> test)

unifyIsCommunative :: IO ()
unifyIsCommunative = do
  void $
    withTyCheckM defaultTyCheckSt $ do
      u <- tyTerm $ Filled unitF
      x <- tyTerm $ Filled $ RtArrF u u
      i0 <- tyTerm $ Empty
      y <- tyTerm $ Filled $ RtArrF i0 i0
      unify x y
      xVal <- liftIO $ gatherTy x
      yVal <- liftIO $ gatherTy y
      liftIO $ xVal @?= RtArr unit unit
      liftIO $ yVal @?= RtArr unit unit

errorFilter :: IO ()
errorFilter = do
  let st =
        defaultTyCheckSt
          { errors = [(AmbiguousTypes (Branch 1), Branch 0)]
          }
  validErrors [Branch 0, Branch 1] st @?= [(AmbiguousTypes (Branch 1), Branch 0)]

unitEvalsToUnit :: IO ()
unitEvalsToUnit = do
  eval unit @?= unit

checkingErrorsGoThrough :: IO ()
checkingErrorsGoThrough = do
  input <- testParse "foo : ()"
  let setup = do
        t <- tyTerm $ Filled RtTyF
        void $ assuming "foo" $ Other $ TyExpr t unitF
  typeCheck input defaultTyCheckSt setup
    `expectTyErrors` [TypeMismatch, TypeMismatch]

infersArrTy :: IO ()
infersArrTy = do
  input <- testParse "() -> ()"
  typeCheck input defaultTyCheckSt noSetup
    `expectTy` RtTy

infersSymbolTy :: IO ()
infersSymbolTy = do
  input <- testParse "foo"
  let setup = do
        t <- tyTerm $ Filled unitF
        void $ assuming "foo" $ Other $ TyExpr t unitF
  typeCheck input defaultTyCheckSt setup
    `expectTy` unit

lambdasCheck :: IO ()
lambdasCheck = do
  input <- testParse "(\\x -> x) : () -> ()"
  typeCheck input defaultTyCheckSt noSetup
    `expectTy` (RtArr unit unit)

checkAnn :: IO ()
checkAnn = do
  input <- testParse "() : ()"
  typeCheck input defaultTyCheckSt noSetup
    `expectTy` unit

checkAppNonDep :: IO ()
checkAppNonDep = do
  input <- testParse "((\\x -> x) : () -> ()) ()"
  typeCheck input defaultTyCheckSt noSetup
    `expectTy` unit

testLex :: Text -> IO [Token]
testLex input =
  case lexer "test" input of
    Right tokens -> pure tokens
    Left e -> assertFailure ("lex fail: " <> show e)

testParse :: Text -> IO AST
testParse input = do
  parse <$> (testLex input) >>= \case
    ([], report) -> assertFailure ("parse fail: " <> show report)
    (result : _, _) -> pure result

expectTy :: Show a => IO (Either a (LocTree l GatheredF)) -> RtVal -> IO ()
expectTy check expected =
  check >>= \case
    Left actual ->
      assertFailure $
        "Type checking returned these errors:\n"
          <> show actual
          <> "\nBut it was supposed to succeed with:\n"
          <> show expected
    Right (LocTree _ _ (GatheredF actual _)) -> do
      actual @?= expected

expectTyErrors :: (Eq a, Show a) => IO (Either a (LocTree l GatheredF)) -> a -> IO ()
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
    Right (LocTree _ _ (GatheredF result _)) -> do
      assertFailure $
        "Type checking returned with:\n"
          <> show result
          <> "\nBut these errors were expected:\n"
          <> show expected
