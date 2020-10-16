{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import AST.Core
import Control.Applicative
import Control.Monad.Free
import Control.Monad.Primitive
import Data.Functor.Foldable (Fix)
import Data.Sum
import Display
import Eval.Stages
import Lib
import Logic
import Logic.Propagator
import Test.Tasty
import Test.Tasty.HUnit
import TypeCheck.Check
import Prelude hiding (product)

main :: IO ()
main = defaultMain tests
  where
    tests = testGroup "Jelly" [evalTests, typeCheckTests]

    typeCheckTests = testGroup "TypeCheck" [typeMergeTests, unifyTests, checkTests]
      where
        typeMergeTests =
          testGroup
            "TypeMerge"
            [ testCase "a ~ b" $ do
                a <- MkTypeMerge <$> newHole
                b <- MkTypeMerge <$> newHole
                let answer = Info b
                let result = merge (OldInfo a) (NewInfo b)
                result @?= answer,
              testCase "(a -> b) ~ (Int -> b)" $ do
                a <- newHole
                b <- newHole
                let old = MkTypeMerge $ a -:> b
                let new = MkTypeMerge $ intTy -:> b
                let result = merge (OldInfo old) (NewInfo new)
                result @?= Info new,
              testCase "(a -> b) ~ (a -> b)" $ do
                a <- newHole
                b <- newHole
                let old = MkTypeMerge $ a -:> b
                let new = MkTypeMerge $ a -:> b
                let result = merge (OldInfo old) (NewInfo new)
                result @?= NoInfo
            ]
        checkTests =
          testGroup
            "Check"
            [ testCase "0 : Int" $ do
                let term = int 0 :: Partial Hole
                let answer = intTy :: Partial Hole
                let st = initCheckST
                runTypeCheck st term >>= \case
                  (Info (MkTypeMerge result) : _) -> result @?= answer
                  _ -> undefined,
              testCase "(\\x -> x) : a -> a" $
                do
                  let term = lam "x" (free "x") :: Partial Hole
                  a <- newHole
                  let answer = a -:> a :: Partial Hole
                  let st = initCheckST
                  runTypeCheck st term >>= \case
                    (Info (MkTypeMerge result) : _) ->
                      case result of
                        Free x -> case project x of
                          Just (Arr a b) -> assertBool "input and output did not match" (a == b)
                          _ -> assertFailure "resulting type wasn't an arrow"
                        _ -> assertFailure "resulting type was a hole"
                    _ -> assertFailure "no typing results",
              testCase "(\\x -> x) 0 : Int" $ do
                let term = lam "x" (free "x") `app` int 0 :: Partial Hole
                let answer = intTy :: Partial Hole
                let st = initCheckST
                runTypeCheck st term >>= \case
                  (Info (MkTypeMerge result) : _) -> result @?= answer
                  _ -> undefined
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

    -- testCase "Unit with Unit" $ do
    --   result <- observeAllT $ do
    --     uni <- unify (struct []) (struct [])
    --     zonk (undefined) (unfix uni)
    --   result @?= [struct []]

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
