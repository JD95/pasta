{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Data.IORef (IORef)
import Lib
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Options

main :: IO ()
main = withArgs ["--hide-successes"] $ defaultMain $ tests
  where
    tests = testGroup "Jelly" [runtime]

    runtime = testGroup "runtime" [propagation]

    propagation =
      testGroup
        "propagation"
        [ testCase "inform fills value" $ do
            let xUpdate =
                  -- Pull values from a tuple and swap them
                  -- f (x, y) = (y, x)
                  rtProd [rtVar 0 `rtIndex` 1, rtVar 0 `rtIndex` 0]
            let xTop = true
            let xCell = rtCell xUpdate xTop
            x <- newRef $ rtBox xCell
            void $ rtEval @IO @IORef x mempty
        ]
