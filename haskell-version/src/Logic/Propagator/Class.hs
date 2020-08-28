{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Logic.Propagator.Class where

import Control.Monad
import Control.Monad.Fail
import Control.Monad.Primitive
import Data.Hashable
import Data.List.NonEmpty
import GHC.Prim
import GHC.Types
import Logic.Info

-- | A container for an alert to schedule
-- with an Id so they can be identified.
data Alert m where
  Alert :: Network m => NetworkId m -> m () -> Alert m

class Network m => HasTargets m f where
  getTargets :: f a -> m [Alert m]
  addTarget :: Alert m -> f a -> m ()
  removeTarget :: NetworkId m -> f a -> m ()

-- | Add some Info to the contained value
class (Network m, Monad m) => Inform m f where
  inform :: Merge a => Info a -> f a -> m ()

-- | An informable value with targets
class (Inform m cell, HasTargets m cell, Monad m) => Cell m cell where
  content :: cell a -> m (Info a)

data SomeCell m where
  SomeCell :: Cell m f => f a -> SomeCell m

class (Eq (NetworkId m), Monad m) => Network m where
  type NetworkId m :: *
  newId :: m (NetworkId m)

  -- | A means to schedule an alert for
  -- some action. In the context of a
  -- propagator network, this schedules
  -- a propagator to fire.
  alert :: m () -> m ()

  propagate :: (Inform m f, Merge a) => m (Info a) -> [f a] -> m ()
  solve :: m ()

data Unique s = Unique !Int (MutableByteArray# s)

type UniqueM m = Unique (PrimState m)

instance Eq (Unique s) where
  Unique _ p == Unique _ q = isTrue# (sameMutableByteArray# p q)

instance Hashable (Unique s) where
  hash (Unique i _) = i
  hashWithSalt d (Unique i _) = hashWithSalt d i

newUnique :: PrimMonad m => m (UniqueM m)
newUnique = primitive $ \s -> case newByteArray# 0# s of
  (# s', ba #) -> (# s', Unique (I# (addr2Int# (unsafeCoerce# ba))) ba #)

instance Network IO where
  type NetworkId IO = UniqueM IO

  newId = newUnique

  alert = id

  propagate action targets = do
    action >>= \case
      NoInfo -> pure ()
      Info x -> forM_ targets $ inform (Info x)
      Contradiction -> error "Boom!"

  solve = pure ()

watchCells ::
  (MonadFail m, Network m) =>
  NonEmpty (SomeCell m) ->
  -- | Action to take when all cells have info
  m () ->
  m ()
watchCells (SomeCell c :| cs) action = do
  propId <- newId
  let go (SomeCell ref) next = do
        content ref >>= \case
          NoInfo -> addTarget (Alert propId (fire propId ref next)) ref
          Info _ -> fire propId ref next
          Contradiction -> error "Boom!"
  let watchAll = do
        forM_ cs $ \(SomeCell ref) -> addTarget (Alert propId action) ref
        action
  addTarget (Alert propId (foldr go watchAll cs)) c

fire :: (Cell m cell) => NetworkId m -> cell x -> m () -> m ()
fire propId ref next = removeTarget propId ref *> next
