{-# LANGUAGE MultiParamTypeClasses #-}

module Runtime.Ref where

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
modifyRef r f = writeRef r =<< f <$> readRef r
