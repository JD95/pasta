{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.EvalEnv where

import           Control.Monad.State.Strict
import           Data.Functor.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Core
import           Env

newtype EvalEnv a
  = EvalEnv
  { runEvalEnv :: State (Map String (Fix CoreE)) a
  } deriving (Functor, Applicative, Monad, MonadState (Map String (Fix CoreE)))

instance SymLookup String (Fix CoreE) EvalEnv where
  symLookup key = Map.lookup key <$> get 
