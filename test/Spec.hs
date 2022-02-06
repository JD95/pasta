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
import Control.Monad.Logic
import Data.List.NonEmpty
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Lexer (Token (..), lexer)
import Parser
import Runtime
import Runtime.Prop
import Runtime.Ref
import Runtime.Term
import Runtime.Types
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit
import TypeCheck
import TypeCheck.Types

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
          testCase "arrows parse" parseArrNonDep,
          testCase "dependent arrows parse" parseArrDep,
          testCase "annotations have highest parsing priority" annHasPriorityOverArrow,
          testCase "products parse" productsParse,
          testCase "indented application inputs parse" appIndent,
          testCase "indented annotations parse" annIndent
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
          testCase "non-dependent function application checks" checkAppNonDep,
          testCase "merging filled values into ambiguous values works" mergeFilledIntoAmb,
          testCase "merging ambiguous values into filled values works" mergeAmbIntoFilled,
          testCase "dependent arrows check" checkArrDep,
          testCase "dependent arrows eval properly" checkDepTyEval
        ]

appIndent :: IO ()
appIndent =
  do
    result <-
      testParse $
        Text.unlines
          [ "foo",
            " a",
            " b"
          ]
    spine result @?= App (Symbol "foo") [Symbol "a", Symbol "b"]

annIndent :: IO ()
annIndent =
  do
    result <-
      testParse $
        Text.unlines
          [ "foo",
            "  :  a",
            "  -> b",
            "  -> c"
          ]
    spine result @?= Ann (Symbol "foo") (Arr Nothing (Symbol "a") (Arr Nothing (Symbol "b") (Symbol "c")))

exampleParses :: IO ()
exampleParses = do
  void $ testParse "let id = \\x -> (\\y -> x) in (id y : t)"

unitParses :: IO ()
unitParses = do
  result <- testParse "()"
  spine result @?= Prod []

parseArrNonDep :: IO ()
parseArrNonDep = do
  result <- testParse "Type -> Type"
  spine result @?= Arr Nothing (Symbol "Type") (Symbol "Type")

parseArrDep :: IO ()
parseArrDep = do
  result <- testParse "(A : Type) -> A -> A"
  spine result @?= Arr (Just "A") (Symbol "Type") (Arr Nothing (Symbol "A") (Symbol "A"))

annParses :: IO ()
annParses = do
  result <- testParse "() : ()"
  spine result @?= (Prod [] `Ann` Prod [])

annHasPriorityOverArrow :: IO ()
annHasPriorityOverArrow = do
  result <- testParse "foo : () -> ()"
  spine result @?= (AST.Symbol "foo" `Ann` (Arr Nothing unitE unitE))

productsParse :: IO ()
productsParse = do
  result <- testParse "(x,y,z)"
  spine result @?= (Prod [Symbol "x", Symbol "y", Symbol "z"])

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
            xVal <- unambiguous "xVal" =<< gatherTy x
            yVal <- unambiguous "yVal" =<< gatherTy y
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
      liftIO $ do
        xVal <- unambiguous "xVal" =<< gatherTy x
        xVal @?= RtArr unit unit
        yVal <- unambiguous "yVal" =<< gatherTy y
        yVal @?= RtArr unit unit

mergeFilledIntoAmb :: IO ()
mergeFilledIntoAmb = void $
  withTyCheckM defaultTyCheckSt $ do
    let old = Old $ Ambiguous (RtTyF :| [unitF])
    let new = New $ Filled RtTyF
    ifte
      (merge @_ @(Hole (RtValF TyTerm)) old new)
      ( \case
          Gain (Filled RtTyF) -> pure ()
          _ -> liftIO $ assertFailure "nope"
      )
      (liftIO $ assertFailure "merge failed")

mergeAmbIntoFilled :: IO ()
mergeAmbIntoFilled = void $
  withTyCheckM defaultTyCheckSt $ do
    let old = Old $ Filled RtTyF
    let new = New $ Ambiguous (RtTyF :| [unitF])
    ifte
      (merge @_ @(Hole (RtValF TyTerm)) old new)
      ( \case
          None -> pure ()
          Conflict -> liftIO $ assertFailure "Conflict!"
          _ -> liftIO $ assertFailure "nope"
      )
      (liftIO $ assertFailure "merge failed")

unambiguous :: String -> (a, [b]) -> IO a
unambiguous _ (x, []) = pure x
unambiguous name (_, _ : _) = assertFailure $ name <> " was ambiguous"

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
        assuming' "foo" $ Other $ TyExpr t unitF
  typeCheck input defaultTyCheckSt setup
    `expectTyErrors` [someTypeMismatch]

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
        assuming' "foo" $ Other $ TyExpr t unitF
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
  input <- testParse "B x"
  let setup = do
        t <- tyTerm $ Filled $ RtTyF
        b <- tyTerm $ Filled $ RtArrF t t
        assuming' "B" $ Other $ TyExpr b unitF
        assuming' "x" $ Other $ TyExpr t unitF
  typeCheck input defaultTyCheckSt setup
    `expectTy` RtTy

checkArrDep :: IO ()
checkArrDep = do
  input <- testParse "(A : Type) -> (B : A -> Type) -> (x : A) -> B x -> (A, B x)"
  typeCheck input defaultTyCheckSt noSetup
    `expectTy` RtTy

checkAnnEval :: IO ()
checkAnnEval = do
  input <- testParse "foo : (\\x -> x) Type"
  let setup = do
        t <- tyTerm $ Filled RtTyF
        assuming' "foo" $ Other $ TyExpr t unitF
  typeCheck input defaultTyCheckSt setup
    `expectTy` RtTy

checkDepTyEval :: IO ()
checkDepTyEval = do
  input <- testParse "foo : (a : Type) -> (\\x -> x) a -> a"
  let setup = do
        x0 <- tyTerm $ Filled $ RtDepTyF 0
        inTy <- tyTerm $ Filled $ RtTyF
        outTy <- tyTerm $ Filled $ RtArrF x0 x0
        full <- tyTerm $ Filled $ RtArrF inTy outTy
        assuming' "foo" $ Other $ TyExpr full unitF
  typeCheck input defaultTyCheckSt setup
    `expectTy` (RtArr RtTy (RtArr (RtDepTy 0) (RtDepTy 0)))

testLex :: Text -> IO [Token]
testLex input =
  case lexer "test" input of
    Right tokens -> pure tokens
    Left e -> assertFailure ("lex fail: " <> show e)

testParse :: Text -> IO AST
testParse input = do
  tokens <- testLex input
  case parse tokens of
    ([], report) -> assertFailure (unpack $ "parse fail: " <> displayReport tokens report)
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

expectTyErrors :: IO (Either [TCError] (LocTree l GatheredF)) -> [ExpectedErr] -> IO ()
expectTyErrors check expected =
  check >>= \case
    Left actual ->
      if matchesErrExpectation expected actual
        then pure ()
        else
          assertFailure $
            "Type checking returned these errors:\n"
              <> show actual
              <> "\nBut these errors were expected:\n"
              <> show (msg <$> expected)
    Right (LocTree _ _ (GatheredF result _)) -> do
      assertFailure $
        "Type checking returned with:\n"
          <> show result
          <> "\nBut these errors were expected:\n"
          <> show (msg <$> expected)

matchesErrExpectation :: [ExpectedErr] -> [TCError] -> Bool
matchesErrExpectation [] [] = True
matchesErrExpectation [] (_ : _) = False
matchesErrExpectation (e : es) actual = go actual []
  where
    go [] _ = False
    go (x : xs) prev =
      if Main.pred e x
        then matchesErrExpectation es (prev <> xs)
        else go xs prev

data ExpectedErr = ExpectedErr {msg :: String, pred :: (TCError -> Bool)}

someTypeMismatch :: ExpectedErr
someTypeMismatch = ExpectedErr "TypeMismatch" $ \case
  TypeMismatch _ _ _ _ -> True
  _ -> False
