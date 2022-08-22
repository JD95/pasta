{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Runtime.Ref where

import Data.Kind
import Control.Applicative
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

class Monad m => MonadRef m where
  type Ref m :: Type -> Type
  newRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()

instance MonadRef IO where
  type Ref IO = IORef
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

modifyRef :: MonadRef m => Ref m a -> (a -> a) -> m ()
modifyRef ref f = do
  value <- readRef ref
  writeRef ref (f value)

backTrackingWriteRef :: (Alternative m, MonadRef m) => Ref m a -> a -> m ()
backTrackingWriteRef ref new = do
  old <- readRef ref
  writeRef ref new <|> (writeRef ref old *> empty)

backTrackingModifyRef :: (Alternative m, MonadRef m) => Ref m a -> (a -> a) -> m ()
backTrackingModifyRef ref f = do
  old <- readRef ref
  modifyRef ref f <|> (writeRef ref old *> empty)
