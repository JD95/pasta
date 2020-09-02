{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Key
  ( Key,
    newKey,
    Box (Lock),
    unlock,
  )
where

import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Proxy
import Data.Type.Coercion
import Data.Type.Equality
import Unsafe.Coerce

-- move to Equality.Key?
newtype Key m a = Key (MutVar (PrimState m) (Proxy a))
  deriving (Eq)

type role Key nominal nominal

instance TestEquality (Key m) where
  testEquality (Key s) (Key t)
    | s == unsafeCoerce t = Just (unsafeCoerce Refl)
    | otherwise = Nothing
  {-# INLINE testEquality #-}

instance TestCoercion (Key m) where
  testCoercion (Key s :: Key m a) (Key t)
    | s == unsafeCoerce t = Just $ unsafeCoerce (Coercion :: Coercion a a)
    | otherwise = Nothing
  {-# INLINE testCoercion #-}

newKey :: PrimMonad m => m (Key m a)
newKey = Key <$> newMutVar Proxy
{-# INLINE newKey #-}

data Box m where
  Lock :: {-# UNPACK #-} !(Key m a) -> a -> Box m

unlock :: Key m a -> Box m -> Maybe a
unlock k (Lock l x) = case testEquality k l of
  Just Refl -> Just x
  Nothing -> Nothing
{-# INLINE unlock #-}
