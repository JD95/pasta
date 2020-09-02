module Store where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Primitive
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Key
import Unique

data StoreKey m a = StoreKey (UniqueM m) (Key m a)

data Store m = Store (HashMap (UniqueM m) (Box m))

insert :: (PrimMonad m) => a -> Store m -> m (Store m, StoreKey m a)
insert val (Store store) = do
  u <- newUnique
  k <- newKey
  let box = Lock k val
  pure $ (Store $ Map.insert u box store, StoreKey u k)

delete :: (PrimMonad m) => StoreKey m a -> Store m -> Store m
delete (StoreKey u _) (Store store) = Store $ Map.delete u store

lookup :: (PrimMonad m) => StoreKey m a -> Store m -> Maybe a
lookup (StoreKey u k) (Store store) = unlock k =<< Map.lookup u store
