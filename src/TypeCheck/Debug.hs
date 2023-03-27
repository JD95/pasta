{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module TypeCheck.Debug where

{-
import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
import Runtime.Types
import System.Console.ANSI
import TypeCheck.Types

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

debugTypeChecking :: Bool
debugTypeChecking = False

debug :: String -> TyCheckM ()
debug msg =
  when debugTypeChecking $ do
    depth <- problemDepth <$> get
    Branch b <- currentBranch <$> get
    liftIO
      . putStrLn
      $ show b <> ": " <> replicate (fromIntegral $ depth * 2) ' ' <> msg

debugErr :: String -> TyCheckM ()
debugErr msg = do
  when debugTypeChecking $ do
    liftIO $ setSGR [SetColor Foreground Vivid Red]
    debug msg
    liftIO $ setSGR [Reset]

debugNoFormat :: String -> TyCheckM ()
debugNoFormat msg =
  when debugTypeChecking $ do
    liftIO . putStrLn $ msg

debugShowTreeTy :: TyTree -> TyCheckM ()
debugShowTreeTy tree = do
  when debugTypeChecking $ do
    let val = treeStripTypes tree
    result <- liftIO (treeGatherRootTy tree)
    debug $ "type of '" <> displayRtVal val <> "' is '" <> displayRtVal result <> "'"

subProblem :: String -> TyCheckM b -> TyCheckM b
subProblem msg check = do
  if debugTypeChecking
    then do
      bsTxt <- displayBindings =<< bindings <$> get
      liftIO $ setSGR [SetColor Foreground Vivid Green]
      debug $ msg <> " " <> bsTxt
      liftIO $ setSGR [Reset]
      modify $ \st -> st {problemDepth = problemDepth st + 1}
      result <- check
      modify $ \st -> st {problemDepth = problemDepth st - 1}
      pure result
    else check
-}
