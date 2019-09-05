{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Subst where

import           Data.Functor.Foldable

class (Eq k, Functor f) => Subst f k where
  depth :: f a -> k -> k
  getKey :: f a -> Maybe k

subst :: Subst f k => Fix f -> k -> Fix f -> Fix f
subst sub n target = (cata go $ target) n
 where
  go x =
    let prop j = Fix $ fmap (flip ($) (depth x j)) x
    in  case getKey x of
          Nothing -> prop
          Just i  -> \m -> if i == m then sub else prop m
