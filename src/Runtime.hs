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
import Data.Functor.Foldable (cata)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Debug.Trace
import Numeric.Natural
import Runtime.Dsl
import Runtime.Log
import Runtime.Ref
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
