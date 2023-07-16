{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Test.Runtime where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logic
import Data.IORef
import Data.List.NonEmpty
import Debug.Trace
import System.Environment
import System.Mem.StableName
import Test.Tasty
import Test.Tasty.HUnit

tests = testGroup "runtime" []

expectException action = do
  try action >>= \case
    Right _ -> assertFailure "Was expecting exception!"
    Left (SomeException _) -> pure ()
