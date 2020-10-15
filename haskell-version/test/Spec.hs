{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import AST.Core
import Control.Applicative
import Control.Monad.Primitive
import Data.Functor.Foldable
import Display
import Eval.Stages
import Lib
import Logic
import Logic.Propagator
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (product)

main :: IO ()
main = defaultMain tests
  where
    tests = testGroup "Jelly" [evalTests, typeCheckTests]

    typeCheckTests = testGroup "TypeCheck" [unifyTests]
      where
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
