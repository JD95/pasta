{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module TypeCheck.Debug where

import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
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

debugNoFormat :: String -> TyCheckM ()
debugNoFormat msg =
  when debugTypeChecking $ do
    liftIO . putStrLn $ msg

debugShowTreeTy :: TyTree -> TyCheckM ()
debugShowTreeTy tree = do
  let val = treeStripTypes tree
  result <-
    liftIO (treeGatherRootTy tree) <&> \case
      One t -> show t
      Many _ -> "ambiguous"
  debug $ "type of " <> show val <> " is " <> result

subProblem :: String -> TyCheckM b -> TyCheckM b
subProblem msg check = do
  debug msg
  modify $ \st -> st {problemDepth = problemDepth st + 1}
  result <- check
  modify $ \st -> st {problemDepth = problemDepth st - 1}
  pure result
