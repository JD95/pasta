{-# LANGUAGE OverloadedStrings #-}

module Repl where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Monad
import System.FSNotify

repl :: IO ()
repl = do
  events <- newChan

  _ <- forkIO . forever $ do
    print =<< readChan events

  withManager $ \mgr -> do
    watchDir mgr "." (const True) (writeChan events)
    forever $ threadDelay 1000000
