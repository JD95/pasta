module Constraint where

data Hole = Hole String

data F = ClassC [Hole]
       | EqC Hole Hole
