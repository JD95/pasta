{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Test.Runtime where

import Control.Exception
import Test.Tasty.HUnit

expectException :: IO a -> IO ()
expectException action = do
  try action >>= \case
    Right _ -> assertFailure "Was expecting exception!"
    Left (SomeException _) -> pure ()
