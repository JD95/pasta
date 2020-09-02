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
    tests = testGroup "Jelly" [evalTests, propTest]

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

    propTest =
      testGroup
        "Propagators"
        [ testCase "values prop forward" $ do
            result <- primToIO $ do
              x <- newPrimCell @IO @Double
              y <- newPrimCell
              z <- newPrimCell
              inform (Info 1) x
              inform (Info 2) y
              adder x y z
              solve
              content z
            result @?= (Info 3.0),
          testCase "values prop backward" $ do
            result <- primToIO $ do
              x <- newPrimCell @IO @Double
              y <- newPrimCell
              z <- newPrimCell
              inform (Info 1) x
              inform (Info 3) z
              adder x y z
              solve
              content y
            result @?= (Info 2.0),
          testCase "farhenheitToCelsius" $ do
            let fahrenheitToCelsius f c = do
                  thirtyTwo <- newPrimCell
                  f32 <- newPrimCell
                  five <- newPrimCell
                  c9 <- newPrimCell
                  nine <- newPrimCell
                  inform (Info 32) thirtyTwo
                  inform (Info 5) five
                  inform (Info 9) nine
                  adder thirtyTwo f32 f
                  product f32 five c9
                  product c nine c9
            result <- primToIO $ do
              f <- newPrimCell @IO @Double
              c <- newPrimCell
              fahrenheitToCelsius f c
              inform (Info 25) c
              solve
              content f
            result @?= (Info 77.0),
          testGroup
            "PrimCells"
            [ testCase "primCells backtrack" $ do
                result <- observeT $ do
                  x <- newPrimCell @_ @Double
                  let path1 = inform (Info 0) x *> empty
                  let path2 = inform (Info 1) x *> inform (Info 3) x
                  let path3 = inform (Info 2) x
                  path1 <|> path2 <|> path3
                  content x
                result @?= Info 2,
              testCase "alternate paths work" $ do
                result <- observeAllT $ do
                  x <- newPrimCell @_ @Double
                  y <- newPrimCell @_ @Double
                  z <- newPrimCell @_ @Double
                  adder x y z
                  let path1 = inform (Info 0) x *> inform (Info 1) y
                  let path2 = inform (Info 1) x *> inform (Info 2) y
                  let path3 = inform (Info 2) x *> inform (Info 3) y
                  path1 <|> path2 <|> path3
                  content z
                result @?= [Info 1, Info 3, Info 5]
            ]
        ]
