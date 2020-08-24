{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logic.Propagator.IOCell where

import Control.Monad.IO.Class
import Data.IORef
import Logic.Info
import Logic.Propagator.Class

data IOCell m a = IOCell (IORef (Info a, [Targets m a]))

instance (MonadAlert m, MonadIO m) => Inform m (IOCell m) where
  inform x (IOCell ref) = do
    (y, _) <- liftIO $ readIORef ref
    case x <> y of
      NoInfo -> undefined
      Info _ -> undefined
      Contradiction -> undefined

instance (MonadAlert m, MonadIO m) => Cell m (IOCell m) where
  content (IOCell ref) = fmap fst . liftIO . readIORef $ ref

instance (MonadIO m) => HasTargets m (IOCell m) where
  getTargets (IOCell ref) = fmap snd . liftIO . readIORef $ ref
  addTarget t (IOCell ref) = liftIO $ modifyIORef ref (\(val, ts) -> (val, t : ts))

newIOCell :: (MonadIO m) => forall a. m (IOCell m a)
newIOCell = fmap IOCell . liftIO $ newIORef (NoInfo, [])
