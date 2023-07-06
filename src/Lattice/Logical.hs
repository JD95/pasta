{-# LANGUAGE TypeFamilies #-}

module Perle.Lattice.Logical where

import Data.Kind
import Data.Void

import Perle.Lattice

class Lattice l => Logical l where
  type Realized l :: Type
  assert :: Realized l -> l
  realize :: l -> [Realized l]

instance Logical () where
  type Realized () = ()
  assert = id
  realize = (: [])
