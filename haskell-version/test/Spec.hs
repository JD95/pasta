{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Data.IORef (IORef)
import qualified Data.Vector as Vec
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
            -- Setup the cell
            let xUpdate = rtVar 0
            let xTop = true
            let xCell = rtCell xUpdate xTop
            x <- newRef $ rtBox xCell
            -- Ensure that the created cell is empty
            rtEval @IO @IORef x mempty >>= readRef >>= \case
              RtProp _ _ _ cellRef _ -> do
                readRef cellRef >>= \case
                  Empty -> pure ()
                  _ -> error "Expecting Empty"
            -- Inform the cell
            code <- newRef . rtBox $ RtInformCell (rtVar 0) (rtInt 5)
            -- Ensure that the cell is now partially filled
            rtEval @IO @IORef code (Vec.fromList [x]) >>= readRef >>= \case
              RtProp _ _ _ cellRef _ -> do
                readRef cellRef >>= \case
                  Partial _ -> pure ()
                  _ -> error "Expecting Partial"
        ]
