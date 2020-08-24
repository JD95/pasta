{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logic.Propagator.Class where

import Logic.Info

-- | A means to schedule an alert for
-- some action. In the context of a
-- propagator network, this schedules
-- a propagator to fire.
class Monad m => MonadAlert m where
  alert :: m () -> m ()

-- | A container for an alert to schedule
-- with an Id so they can be identified.
data Alert m a where
  Alert :: MonadAlert m => m () -> Int -> Alert m a

-- | A collection of informable cells
-- along with the means to inform them.
data Targets m a where
  T :: Inform m f => [f a] -> Targets m a

class HasTargets m f where
  getTargets :: f a -> m [Targets m a]
  addTarget :: Targets m a -> f a -> m ()

-- | Add some Info to the contained value
class (MonadAlert m, Monad m) => Inform m f where
  inform :: Merge a => Info a -> f a -> m ()

-- | An informable value with targets
class (Inform m cell, HasTargets m cell, Monad m) => Cell m cell where
  content :: cell a -> m (Info a)

class (MonadAlert m, Monad m) => Network m where
  propagate :: Inform m f => m (Info a) -> [f a] -> m ()
  solve :: m ()

instance MonadAlert IO where
  alert = undefined

instance Network IO where
  propagate = undefined
  solve = undefined
