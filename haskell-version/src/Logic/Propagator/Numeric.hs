{-# LANGUAGE DataKinds #-}

module Logic.Propagator.Numeric where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Primitive
import Data.Sequence (Seq (..))
import Logic.Propagator

adder ::
  (PrimMonad m, Eq a, Merge a, Num a) =>
  CellRef m a ->
  CellRef m a ->
  CellRef m a ->
  Eff '[State (Seq (PropRef m)), m] ()
adder in1 in2 = liftProp . propagator (\[x, y] -> x + y) [in1, in2]

subtractor ::
  (PrimMonad m, Eq a, Merge a, Num a) =>
  CellRef m a ->
  CellRef m a ->
  CellRef m a ->
  Eff '[State (Seq (PropRef m)), m] ()
subtractor in1 in2 = liftProp . propagator (\[x, y] -> x - y) [in1, in2]

multiplier ::
  (PrimMonad m, Eq a, Merge a, Num a) =>
  CellRef m a ->
  CellRef m a ->
  CellRef m a ->
  Eff '[State (Seq (PropRef m)), m] ()
multiplier in1 in2 = liftProp . propagator (\[x, y] -> x * y) [in1, in2]

divider ::
  (PrimMonad m, Eq a, Merge a, Floating a) =>
  CellRef m a ->
  CellRef m a ->
  CellRef m a ->
  Eff '[State (Seq (PropRef m)), m] ()
divider in1 in2 = liftProp . propagator (\[x, y] -> x / y) [in1, in2]

sum ::
  (PrimMonad m, Eq a, Merge a, Num a) =>
  CellRef m a ->
  CellRef m a ->
  CellRef m a ->
  Eff '[State (Seq (PropRef m)), m] ()
sum in1 in2 out = do
  adder in1 in2 out
  subtractor out in1 in2
  subtractor out in2 in1

product ::
  (PrimMonad m, Eq a, Merge a, Floating a) =>
  CellRef m a ->
  CellRef m a ->
  CellRef m a ->
  Eff '[State (Seq (PropRef m)), m] ()
product in1 in2 out = do
  multiplier in1 in2 out
  divider out in1 in2
  divider out in2 in1
