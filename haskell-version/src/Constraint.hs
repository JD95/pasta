module Constraint where

data F a = EqC a a

data W a = Flat (F a)

(~:) x y = Flat (EqC x y)
