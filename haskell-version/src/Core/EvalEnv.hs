{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.EvalEnv where

import           Data.Functor.Foldable
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Polysemy
import           Polysemy.State

import           Core
import           Env

runSymLookupState
  :: (Ord key, Member (State (Map key val)) r)
  => Sem (SymLookup key val ': r) a
  -> Sem r a
runSymLookupState = interpret $ \case
  SymLookup key -> Map.lookup key <$> get
