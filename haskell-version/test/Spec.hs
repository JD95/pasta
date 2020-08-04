{-# LANGUAGE TypeApplications #-}

import AST.Core
import Eval.Stages
import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests
  where
    tests = testGroup "Eval" [evalTest, boundTest]

    evalTest = testCase "eval int is int" $ do
      result <- runNormal [] . term . int $ 1
      result @?= nf (int 1)

    boundTest =
      testGroup
        "bound"
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
