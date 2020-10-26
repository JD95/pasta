{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import AST.Core
import Control.Applicative
import Control.Monad.Free
import Control.Monad.Primitive
import Data.Functor.Foldable (Fix)
import Data.Maybe
import Data.Sum
import Display
import Eval.Stages
import Lib
import Logic
import Logic.Propagator
import Parser.Lexer
import Test.Tasty
import Test.Tasty.HUnit
import TypeCheck.Typed
import Prelude hiding (pi, product)

main :: IO ()
main = defaultMain tests
  where
    tests = testGroup "Jelly" [parsingTests, evalTests, typeCheckTests]

    parsingTests = testGroup "Parsing" [lexingTests]
      where
        lexingTests =
          testGroup
            "Lexing"
            [ testCase "Natural Number" $ do
                let result = natNum "123"
                let answer = Just $ TNat 123
                result @?= answer,
              testCase "Integer Number" $ do
                let result = intNum "-123"
                let answer = Just $ TInt (-123)
                result @?= answer,
              testCase "Double Number" $ do
                let result = dblNum "1.23"
                let answer = Just $ TDbl 1.23
                result @?= answer
            ]

    typeCheckTests = testGroup "TypeCheck" [typeMergeTests, unifyTests, checkTests]
      where
        typeMergeTests =
          testGroup
            "TypeMerge"
            [ testCase "a ~ b" $ do
                a <- MkTypeMerge Learned <$> newHole
                b <- MkTypeMerge Learned <$> newHole
                let answer = Info b
                let result = merge (OldInfo a) (NewInfo b)
                result @?= answer,
              testCase "(a -> b) ~ (Int -> b)" $ do
                a <- newHole
                b <- newHole
                let old = MkTypeMerge Learned $ a -:> b
                let new = MkTypeMerge Learned $ intTy -:> b
                let result = merge (OldInfo old) (NewInfo new)
                result @?= Info new,
              testCase "(a -> b) ~ (a -> b)" $ do
                a <- newHole
                b <- newHole
                let old = MkTypeMerge Learned $ a -:> b
                let new = MkTypeMerge Learned $ a -:> b
                let result = merge (OldInfo old) (NewInfo new)
                result @?= NoInfo,
              testCase "(Int -> Int) ~/ (Type 0 -> b)" $ do
                b <- newHole
                let old = intTy -:> intTy
                let new = ty 0 -:> b
                let result = merge (OldInfo $ MkTypeMerge Learned old) (NewInfo $ MkTypeMerge Learned new)
                result @?= (learn $ conflict old new)
            ]
        checkTests =
          testGroup
            "Check"
            [ testCase "0 : Int" $ do
                let term = int 0 :: Partial Hole
                let answer = intTy :: Partial Hole
                let st = initCheckST
                runTypeCheck st term >>= \case
                  (Info (MkTypeMerge _ result) : _) -> result @?= answer
                  _ -> undefined,
              testCase "Holes for unannotated lambdas" $
                do
                  let term = lam "x" (free "x") :: Partial Hole
                  a <- newHole
                  let answer = a -:> a :: Partial Hole
                  let st = initCheckST
                  runTypeCheck st term >>= \case
                    (Info (MkTypeMerge _ result) : _) ->
                      case result of
                        Free x -> case project x of
                          Just (Arr _ a b) -> assertBool "input and output did not match" (a == b)
                          _ -> assertFailure "resulting type wasn't an arrow"
                        _ -> assertFailure "resulting type was a hole"
                    _ -> assertFailure "no typing results",
              testCase "Holes filled by application" $ do
                let term = lam "x" (free "x") `app` int 0 :: Partial Hole
                let answer = intTy :: Partial Hole
                let st = initCheckST
                runTypeCheck st term >>= \case
                  (Info (MkTypeMerge _ result) : _) -> result @?= answer
                  _ -> undefined,
              testCase "Annotated lambdas" $ do
                let term = lam "x" (free "x") `ann` (intTy -:> intTy) :: Partial Hole
                let answer = intTy -:> intTy :: Partial Hole
                let st = initCheckST
                runTypeCheck st term >>= \case
                  (Info (MkTypeMerge _ result) : _) -> result @?= answer
                  _ -> assertFailure "no type results",
              testCase "Identity function" $ do
                let term = lam "a" $ lam "x" $ free "x" :: Partial Hole
                let givenTy = pi "a" (ty 0) (free "a" -:> free "a") :: Partial Hole
                let input = term `ann` givenTy :: Partial Hole
                let answer = givenTy
                let st = initCheckST
                runTypeCheck st input >>= \case
                  (Info (MkTypeMerge _ result) : _) -> result @?= answer
                  _ -> assertFailure "no type results",
              testCase "Application of input to type" $ do
                let exp = lam "a" $ lam "x" $ free "x" :: Partial Hole
                let expTy = pi "in1" (ty 0) $ free "in1" -:> free "in1" :: Partial Hole
                let term = (exp `ann` expTy) `app` intTy :: Partial Hole
                let answer = intTy -:> intTy :: Partial Hole
                let st = initCheckST
                runTypeCheck st term >>= \case
                  (Info (MkTypeMerge _ result) : _) -> result @?= answer
                  _ -> assertFailure "no type results",
              testCase "Const function" $ do
                let exp = lam "a" $ lam "b" $ lam "x" $ lam "y" $ free "x" :: Partial Hole
                let expTy = pi "a" (ty 0) $ pi "b" (ty 0) $ free "a" -:> free "b" -:> free "a" :: Partial Hole
                let term = ((exp `ann` expTy) `app` intTy) `app` natTy :: Partial Hole
                let answer = intTy -:> natTy -:> intTy :: Partial Hole
                let st = initCheckST
                runTypeCheck st term >>= \case
                  (Info (MkTypeMerge _ result) : _) -> result @?= answer
                  _ -> assertFailure "no type results",
              testCase "Const with input" $ do
                let exp = lam "a" $ lam "b" $ lam "x" $ lam "y" $ free "x" :: Partial Hole
                let expTy = pi "a" (ty 0) $ pi "b" (ty 0) $ free "a" -:> free "b" -:> free "a" :: Partial Hole
                let term = (exp `ann` expTy) `app` intTy `app` natTy `app` int 0 :: Partial Hole
                let answer = natTy -:> intTy :: Partial Hole
                let st = initCheckST
                runTypeCheck st term >>= \case
                  (Info (MkTypeMerge _ result) : _) -> result @?= answer
                  _ -> assertFailure "no type results",
              testCase "Errors give result" $ do
                let exp = mismatch (Expected $ free "x") (Given $ free "y") :: Partial Hole
                let st = initCheckST
                runTypeCheck st exp >>= \case
                  (Info (MkTypeMerge _ result) : _) -> pure ()
                  _ -> assertFailure "no type results",
              testCase "Type mismatch gives error" $ do
                let exp = int 0 `ann` ty 0 :: Partial Hole
                let st = initCheckST
                runTypeCheck st exp >>= \case
                  (Info (MkTypeMerge _ result) : _) -> assertBool "should have thrown error" (isJust $ isErr result)
                  _ -> assertFailure "no type results",
              testCase "Wrong input type gives error" $ do
                let exp = ((lam "x" (free "x")) `ann` (intTy -:> intTy)) :: Partial Hole
                let term = exp `app` (ty 0)
                let st = initCheckST
                runTypeCheck st term >>= \case
                  (Info (MkTypeMerge _ result) : _) -> assertBool "should have thrown error" (isJust $ isErr result)
                  _ -> assertFailure "no type results",
              testCase "Application to non-function gives error" $ do
                let exp = free "x" `ann` intTy :: Partial Hole
                let term = exp `app` (ty 0)
                let st = initCheckST
                runTypeCheck st term >>= \case
                  (Info (MkTypeMerge _ result) : _) -> assertBool "should have thrown error" (isJust $ isErr result)
                  _ -> assertFailure "no type results"
            ]
        unifyTests =
          testGroup
            "Unify"
            [ testCase "render holes" $ do
                let term = struct [hole 0, hole 5, hole 2, hole 1] :: Partial Hole
                let answer = asFix $ struct [free "a", free "b", free "c", free "d"] :: Fix Typed
                let result = renderHoles term
                display result @?= display answer
            ]

    evalTests =
      testGroup
        "Eval"
        [ testGroup
            "Int"
            [ testCase "eval is idempotent" $ do
                result <- runNormal [] . term . asFix . int $ 1
                result @?= nf (asFix $ int 1)
            ],
          testGroup
            "Bound"
            [ testCase "can lookup value" $ do
                let r = term . asFix . int $ 1
                let t = term . thunk [r] . bnd $ 0
                result <- runNormal [] t
                result @?= nf (asFix $ int 1),
              testCase "looks up proper value" $ do
                let r0 = term . asFix . int $ 0
                let r1 = term . asFix . int $ 1
                let t = term $ thunk [r0, r1] (bnd 1)
                result <- runNormal [] t
                result @?= nf (asFix $ int 1)
            ]
        ]
