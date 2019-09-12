{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Subst where

import           Control.Arrow
import           Data.Functor.Foldable
import           Numeric.Natural
import           Summable

class (Eq k, Functor f) => Subst f k where
  depth :: f a -> k -> f (k, a)
  getKey :: f a -> Maybe k

instance
  ( Eq k
  , Functor f
  , Subst f k
  , Subst (Summed fs) k
  , Functor (Summed (f ': fs))
  ) => Subst (Summed (f ': fs)) k where

  depth (Here x) k = Here (depth x k)
  depth (There x) k = There (depth x k)

  getKey (Here x) = getKey x
  getKey (There x) = getKey x

instance (Eq k) => Subst (Summed '[]) k where
  depth f n = (const n &&& id) <$> f
  getKey _ = Nothing

subst :: Subst f k => Fix f -> k -> Fix f -> Fix f
subst sub n target = apo go (n, target)
 where
  go (i, Fix x) = case getKey x of
    Nothing -> Right <$> depth x i
    Just j  -> if i == j then Left <$> unfix sub else Right <$> depth x i

newtype Depth = Depth Natural deriving (Eq, Ord, Show)
