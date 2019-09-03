{-# LANGUAGE MultiParamTypeClasses #-}
module Env where

import           Data.Functor.Foldable
import           Data.Functor.Identity
import           Control.Monad.Catch.Pure
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Map.Strict                ( Map )


newtype Env s r e a = Env { runEnv :: StateT s (ReaderT r (CatchT Identity)) a }

class Monad m => Log m where
  info :: String -> m ()

class Monad m => SymLookup key val m where
  symLookup :: key -> m (Maybe val)

class Monad m => SymAlter key val m where
  symAlter  :: key -> (Maybe val -> Maybe val) -> m ()

class Monad m => NameGen m where
  newName :: m String
