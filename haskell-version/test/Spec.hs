{-# LANGUAGE TypeApplications #-}

import AST.Core
import Control.Applicative
import Control.Monad.Primitive
import Eval.Stages
import Lib
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (product)

main :: IO ()
main = defaultMain tests
  where
    tests = testGroup "Jelly" [evalTests]

    evalTests =
      testGroup
        "Eval"
        [ testGroup
            "Int"
            [ testCase "eval is idempotent" $ do
                result <- runNormal [] . term . int $ 1
                result @?= nf (int 1)
            ],
          testGroup
            "Bound"
            [ testCase "can lookup value" $ do
                let r = term . int $ 1
                let t = term . thunk [r] . bnd $ 0
                result <- runNormal [] t
                result @?= nf (int 1),
              testCase "looks up proper value" $ do
                let r0 = term . int $ 0
                let r1 = term . int $ 1
                let t = term $ thunk [r0, r1] (bnd 1)
                result <- runNormal [] t
                result @?= nf (int 1)
            ]
        ]
