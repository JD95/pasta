{-# LANGUAGE DeriveFunctor #-}

module Constraint where

import           Numeric.Natural

data F a
  = EqC a a
  | IsRig a
  | IsPol a
  | InUni a Natural
    deriving (Functor)

data W a = Flat (F a) deriving (Functor)

(~:) x y = Flat (EqC x y)

isRig = Flat . IsRig
isPol = Flat . IsPol

(<:) x n = Flat (InUni x n)
