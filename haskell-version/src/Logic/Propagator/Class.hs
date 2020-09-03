{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Logic.Propagator.Class where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Hashable
import Logic.Info
import Logic.Monad
import Unique

-- | A container for an alert to schedule
-- with an Id so they can be identified.
data Alert m where
  Alert :: Network m => NetworkId m -> m () -> Alert m

instance Eq (Alert m) where
  (Alert x _) == (Alert y _) = x == y

class Network m => HasTargets m f where
  getTargets :: f a -> m [Alert m]
  addTarget :: Alert m -> f a -> m ()
  removeTarget :: NetworkId m -> f a -> m ()

instance Hashable (Alert m) where
  hash (Alert x _) = hash x
  hashWithSalt d (Alert x _) = hashWithSalt d x

-- | Add some Info to the contained value
class (Network m, Monad m) => Inform m f where
  inform :: Merge a => Info a -> f a -> m ()

-- | An informable value with targets
class (Inform m cell, HasTargets m cell, Monad m) => Cell m cell where
  content :: cell a -> m (Info a)

data SomeCell m where
  SomeCell :: Cell m f => f a -> SomeCell m

class (MonadPlus m, Hashable (NetworkId m), Eq (NetworkId m), Monad m) => Network m where
  type NetworkId m :: *
  newId :: m (NetworkId m)

  -- | A means to schedule an alert for
  -- some action. In the context of a
  -- propagator network, this schedules
  -- a propagator to fire.
  alert :: m () -> m ()

  propagate :: (Inform m f, Merge a) => m (Info a) -> [f a] -> m ()
  solve :: m ()

instance Network IO where
  type NetworkId IO = UniqueM IO

  newId = newUnique

  alert = id

  propagate action targets = do
    action >>= \case
      NoInfo -> pure ()
      Info x -> forM_ targets $ inform (Info x)
      Contradiction -> empty

  solve = pure ()

instance Network (LogicT IO) where
  type NetworkId (LogicT IO) = UniqueM (LogicT IO)

  newId = liftIO newUnique

  alert = id

  propagate action targets = do
    action >>= \case
      NoInfo -> pure ()
      Info x -> forM_ targets $ inform (Info x)
      Contradiction -> empty

  solve = pure ()

watchCells ::
  (Network m) =>
  NetworkId m ->
  [SomeCell m] ->
  -- | Action to take when all cells have info
  m () ->
  m ()
watchCells propId cells action = forM_ cells $ \(SomeCell ref) -> addTarget (Alert propId action) ref

waitOn ::
  (Network m) =>
  [SomeCell m] ->
  -- | Action to take when all cells have info
  m () ->
  m ()
waitOn cells action = do
  filterM (\(SomeCell c) -> not . isInfo <$> content c) cells >>= \case
    [] -> action
    (SomeCell c : cs) -> do
      propId <- newId
      let go (SomeCell ref) next = do
            content ref >>= \case
              -- One of the values needed doesn't have info,
              -- watch this value until it fills
              NoInfo -> addTarget (Alert propId (fire propId ref next)) ref
              Info _ -> fire propId ref next
              Contradiction -> error "Boom!"
      let wakeUp = watchCells propId cells action *> action
      addTarget (Alert propId (foldr go wakeUp cs)) c

fire :: (Cell m cell) => NetworkId m -> cell x -> m () -> m ()
fire propId ref next = removeTarget propId ref *> next
