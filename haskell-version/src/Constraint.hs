{-# LANGUAGE DeriveFunctor #-}

module Constraint where

import           Numeric.Natural

import           Display

data F a
  = EqC a a
  | IsRig a
  | IsPol a
  | InUni a Natural
    deriving (Functor)

instance Display a => Display (F a) where
  display (EqC x y) = display x <> " ~ " <> display y
  display _ = "" 

data W a = Flat (F a) deriving (Functor)

instance (Display a) => Display (W a) where
  display (Flat f) = display f

(~:) x y = Flat (EqC x y)

isRig = Flat . IsRig
isPol = Flat . IsPol

(<:) x n = Flat (InUni x n)
