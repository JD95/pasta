{-# LANGUAGE RankNTypes #-}

module Constructors where

import           Data.Functor.Foldable

mkMu :: (forall x . g x -> f x) -> (forall a . (f a -> a) -> g a) -> Mu f
mkMu h g = Mu (\f -> f . h $ g f)
