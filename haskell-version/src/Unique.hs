{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Unique where

import Control.Monad.Primitive
import Data.Hashable
import GHC.Prim
import GHC.Types

data Unique s = Unique !Int (MutableByteArray# s)

type UniqueM m = Unique (PrimState m)

instance Eq (Unique s) where
  Unique _ p == Unique _ q = isTrue# (sameMutableByteArray# p q)

instance Hashable (Unique s) where
  hash (Unique i _) = i
  hashWithSalt d (Unique i _) = hashWithSalt d i

newUnique :: PrimMonad m => m (UniqueM m)
newUnique = primitive $ \s -> case newByteArray# 0# s of
  (# s', ba #) -> (# s', Unique (I# (addr2Int# (unsafeCoerce# ba))) ba #)
