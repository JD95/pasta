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

import           Data.Functor.Foldable
import           Numeric.Natural
import           Summable

class (Eq k, Functor f) => Subst f k where
  depth :: f a -> k -> k
  getKey :: f a -> Maybe k

instance
  ( Eq k
  , Functor f
  , Subst f k
  , Subst (Summed fs) k
  , Functor (Summed (f ': fs))
  ) => Subst (Summed (f ': fs)) k where

  depth (Here x) k = depth x k
  depth (There x) k = depth x k

  getKey (Here x) = getKey x
  getKey (There x) = getKey x

instance (Eq k) => Subst (Summed '[]) k where
  depth _ n = n
  getKey _ = Nothing

subst :: Subst f k => Fix f -> k -> Fix f -> Fix f
subst sub n target = (cata go $ target) n
 where
  go x =
    let prop j = Fix $ fmap (flip ($) (depth x j)) x
    in  case getKey x of
          Nothing -> prop
          Just i  -> \m -> if i == m then sub else prop m

newtype Depth = Depth Natural deriving (Eq, Ord, Show)
