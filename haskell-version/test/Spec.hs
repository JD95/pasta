{-# LANGUAGE TypeApplications #-}

import AST.Core
import Control.Monad.Freer
import Data.Proxy
import Eval.Stages
import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests
  where
    tests = testGroup "Eval" [evalTest, boundTest, propTest]

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

    propTest =
      testGroup
        "Propagators"
        [ testCase "values prop forward" $ do
            let result = runPropagator $ do
                  x <- cell
                  y <- cell
                  z <- cell
                  constant 1 x
                  constant 2 y
                  adder x y z
                  solve Proxy
                  send $ content z
            result @?= (Info 3.0),
          testCase "values prop backward" $ do
            let result = runPropagator $ do
                  x <- cell
                  y <- cell
                  z <- cell
                  constant 1 x
                  constant 3 z
                  Lib.sum x y z
                  solve Proxy
                  content y
            result @?= (Info 2.0),
          testCase "farhenheitToCelsius" $ do
            let fahrenheitToCelsius f c = do
                  thirtyTwo <- cell
                  f32 <- cell
                  five <- cell
                  c9 <- cell
                  nine <- cell
                  constant 32 thirtyTwo
                  constant 5 five
                  constant 9 nine
                  Lib.sum thirtyTwo f32 f
                  Lib.product f32 five c9
                  Lib.product c nine c9
            let result = runPropagator $ do
                  f <- cell
                  c <- cell
                  fahrenheitToCelsius f c
                  addContent (Info 25) c
                  solve Proxy
                  content f
            result @?= (Info 77.0)
        ]
