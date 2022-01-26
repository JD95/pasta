{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Runtime where

import Control.Monad
import Control.Monad.State
import Data.Traversable
import Runtime.Types
import Prelude hiding (const, id, log)

data RtEnv = RtEnv {stack :: [RtVal]}

newtype EvalM a = EvalM {runEvalM :: State RtEnv a}
  deriving (Functor, Applicative, Monad, MonadState RtEnv)

eval :: RtVal -> RtVal
eval val = evalState (runEvalM (go val)) (RtEnv [])
  where
    go :: RtVal -> EvalM RtVal
    go (RtProd xs) = RtProd <$> traverse go xs
    go _ = undefined
