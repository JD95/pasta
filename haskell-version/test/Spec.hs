{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import AST.Core
import Control.Monad.Freer
import Control.Monad.Primitive
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
            result <-
              primToIO . runPropagator @IO $ \env -> do
                x <- doubleCell env
                y <- doubleCell env
                z <- doubleCell env
                constant 1 x
                constant 2 y
                adder x y z
                solve env
                send $ content z

            result @?= (Info 3.0),
          testCase "values prop backward" $ do
            result <- runPropagator $ \env -> do
              x <- doubleCell env
              y <- doubleCell env
              z <- doubleCell env
              constant 1 x
              constant 3 z
              Lib.sum x y z
              solve env
              send $ content y
            result @?= (Info 2.0),
          testCase "farhenheitToCelsius" $ do
            let fahrenheitToCelsius f c env = do
                  thirtyTwo <- doubleCell env
                  f32 <- doubleCell env
                  five <- doubleCell env
                  c9 <- doubleCell env
                  nine <- doubleCell env
                  constant 32 thirtyTwo
                  constant 5 five
                  constant 9 nine
                  Lib.sum thirtyTwo f32 f
                  Lib.product f32 five c9
                  Lib.product c nine c9
            result <- runPropagator $ \env -> do
              f <- doubleCell env
              c <- doubleCell env
              fahrenheitToCelsius f c env
              fill (Info 25) c
              solve env
              send $ content f
            result @?= (Info 77.0)
        ]
