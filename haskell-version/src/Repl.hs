{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Repl where

import Control.Concurrent (forkIO)
import Control.Monad
import Lens.Micro.TH
import Parser.Lexer
import RIO
import RIO.Text (isPrefixOf, isSuffixOf, pack)
import System.FSNotify
import Prelude (print)

data Env = Env {_target :: !FilePath}

makeLenses ''Env

runRepl :: FilePath -> IO ()
runRepl dir = runRIO (Env dir) repl

repl :: RIO Env ()
repl = do
  dir <- view target <$> ask
  events <- newChan

  _ <- liftIO . forkIO . forever $ do
    readChan events >>= \case
      Modified p _ _ -> do
        when (not ("#" `isPrefixOf` pack p) && ".jy" `isSuffixOf` pack p) $ do
          content <-
            readFileUtf8 p `catchIO` \e -> do
              pure $ "Could not read from " <> pack p
          print $ lex content
      _ -> pure ()

  liftIO
    . withManager
    $ \mgr -> do
      watchDir mgr dir (const True) (writeChan events)
      forever $ threadDelay 1000000
