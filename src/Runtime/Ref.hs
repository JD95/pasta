{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Runtime.Ref where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Logic
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Mem.StableName

class Monad m => Ref m r where
  newRef :: a -> m (r a)
  readRef :: r a -> m a
  writeRef :: r a -> a -> m ()

data Strict a = Strict {getStrict :: !a}

class GenTag m tag where
  genTag :: Strict a -> m (tag a)

instance GenTag IO StableName where
  genTag = makeStableName . getStrict

instance GenTag (LogicT IO) StableName where
  genTag = liftIO . makeStableName . getStrict

instance MonadIO m => Ref m IORef where
  newRef = liftIO . newIORef
  readRef = liftIO . readIORef
  writeRef ref = liftIO . writeIORef ref

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
