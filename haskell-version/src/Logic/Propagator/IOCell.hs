{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logic.Propagator.IOCell where

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Data.HashSet
import qualified Data.HashSet as Set
import Data.IORef
import Logic.Info
import Logic.Propagator.Class

data IOCell m a = IOCell (IORef (Info a, HashSet (Alert m)))

instance (Network m, MonadIO m) => Inform m (IOCell m) where
  inform x (IOCell ref) = do
    (y, listeners) <- liftIO $ readIORef ref
    case x <> y of
      NoInfo -> pure ()
      Info z -> do
        liftIO $ writeIORef ref (Info z, listeners)
        forM_ listeners $ \(Alert _ go) -> go
      Contradiction -> error "Boom!"

instance (Network m, MonadIO m) => Cell m (IOCell m) where
  content (IOCell ref) = fmap fst . liftIO . readIORef $ ref

instance (Network m, MonadIO m) => HasTargets m (IOCell m) where
  getTargets (IOCell ref) = fmap (Set.toList . snd) . liftIO . readIORef $ ref
  addTarget t (IOCell ref) = liftIO $ modifyIORef ref $ second $ Set.insert t
  removeTarget i (IOCell ref) = liftIO $ modifyIORef ref $ second $ Set.delete (Alert i undefined)

newIOCell :: (MonadIO m) => forall a. m (IOCell m a)
newIOCell = fmap IOCell . liftIO $ newIORef (NoInfo, Set.empty)
