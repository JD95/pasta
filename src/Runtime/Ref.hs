{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Runtime.Ref where

import Control.Applicative
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

class Monad m => Ref m r where
  newRef :: a -> m (r a)
  readRef :: r a -> m a
  writeRef :: r a -> a -> m ()

instance Ref IO IORef where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

modifyRef :: Ref m r => r a -> (a -> a) -> m ()
modifyRef ref f = do
  value <- readRef ref
  writeRef ref (f value)

backTrackingWriteRef :: (Alternative m, Ref m r) => r a -> a -> m ()
backTrackingWriteRef ref new = do
  old <- readRef ref
  writeRef ref new <|> (writeRef ref old *> empty)

backTrackingModifyRef :: (Alternative m, Ref m r) => r a -> (a -> a) -> m ()
backTrackingModifyRef ref f = do
  old <- readRef ref
  modifyRef ref f <|> (writeRef ref old *> empty)
