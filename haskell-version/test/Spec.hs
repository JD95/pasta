{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception (SomeException (..), try)
import Control.Monad
import Data.Functor.Foldable (Fix (..), cata)
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
            x <- newRef . rtBox $ rtCell firstTopMerge
            -- Ensure that the created cell is empty
            rtEval @IO @IORef x mempty >>= readRef >>= \case
              RtCell cellRef _ _ -> do
                readRef cellRef >>= \case
                  RtWhnf (RtCon 0 _) -> pure ()
                  _ -> error "Expecting Empty"
            -- Inform the cell
            code <- newRef . rtBox $ RtInformCell (rtVar 0) (rtInt 5)
            _ <- rtEval @IO @IORef code (Vec.fromList [x])
            -- Ensure that the cell is now filled
            readRef x >>= \case
              RtCell cellRef _ _ -> do
                normalize cellRef >>= \case
                  Fix (RtCon 2 _) -> pure ()
                  _ -> error $ "Expecting Top",
          testCase
            "inform will merge values"
            $ do
              -- Setup the cell
              let xCell = rtCell firstTopMerge
              x <- join $ rtEval @IO @IORef <$> newRef (rtBox xCell) <*> pure mempty
              -- Inform the cell
              code <- newRef . rtBox $ RtInformCell (rtVar 0) (rtInt 5)
              rtEval @IO @IORef code (Vec.fromList [x]) >>= readRef >>= \case
                RtCell cellRef _ _ -> do
                  readRef cellRef >>= \case
                    RtWhnf (RtCon 2 topResult) -> do
                      readRef topResult >>= \case
                        RtWhnf (RtPrim y) -> y @?= RtInt 5
                        RtWhnf _ -> error "Some other whnf"
                        RtCell _ _ _ -> error "Some cell"
                        RtThunk _ _ -> error "Some thunk"
                      -- _ -> error "Expecting Whnf"
                      pure ()
                    _ -> error "Expecting Top"
              -- Inform it again, should cause a conflict
              code' <- newRef . rtBox $ RtInformCell (rtVar 0) (rtInt 5)
              try (rtEval @IO @IORef code' (Vec.fromList [x])) >>= \case
                Left (SomeException _) -> pure ()
                Right _ -> error "Repeated inform should have generated a conflict!"
        ]
