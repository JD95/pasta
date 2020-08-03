{-# LANGUAGE TypeApplications #-}

import AST.Core
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Data.Functor.Foldable
import Data.IORef
import Eval.Stages
import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests
  where
    tests = testGroup "Eval" [evalTest, boundTest]

    evalTest = testCase "eval int is int" $ do
      let t = term $ int 1
      result <- runNormal (Stack []) t
      result @?= int 1

    boundTest =
      testGroup
        "bound"
        [ testCase "can lookup value" $ do
            let val = term $ int 1
            ref <- newIORef val
            let t = term $ thunk (Stack [ref]) (bnd 0)
            result <- runNormal (Stack []) t
            result @?= int 1,
          testCase "looks up proper value" $ do
            let val0 = term $ int 0
            let val1 = term $ int 1
            ref0 <- newIORef val0
            ref1 <- newIORef val1
            let t = term $ thunk (Stack [ref0, ref1]) (bnd 1)
            result <- runNormal (Stack []) t
            result @?= int 1
        ]
