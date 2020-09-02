{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logic.Propagator
  ( module Logic.Propagator.Class,
    adder,
    product,
    liftBinOp,
  )
where

import Control.Monad.Fail
import Logic.Info
import Logic.Propagator.Class
import Prelude hiding (product)

adder :: (Merge a, Num a, MonadFail m, Network m, Cell m f, Cell m g, Cell m h) => f a -> g a -> h a -> m ()
adder in1 in2 out = do
  watchCells [SomeCell in1, SomeCell in2] $ propagate (add in1 in2) [out]
  watchCells [SomeCell out, SomeCell in2] $ propagate (sub out in2) [in1]
  watchCells [SomeCell out, SomeCell in1] $ propagate (sub out in1) [in2]

product :: (Merge a, Floating a, MonadFail m, Network m, Cell m f, Cell m g, Cell m h) => f a -> g a -> h a -> m ()
product in1 in2 out = do
  watchCells [SomeCell in1, SomeCell in2] $ propagate (liftBinOp (*) in1 in2) [out]
  watchCells [SomeCell out, SomeCell in2] $ propagate (liftBinOp (/) out in2) [in1]
  watchCells [SomeCell out, SomeCell in1] $ propagate (liftBinOp (/) out in1) [in2]

add :: (Num a, MonadFail m, Cell m f, Cell m g) => f a -> g a -> m (Info a)
add = liftBinOp (+)

sub :: (Num a, MonadFail m, Cell m f, Cell m g) => f a -> g a -> m (Info a)
sub = liftBinOp (-)

liftBinOp :: (MonadFail m, Cell m f, Cell m g) => (a -> b -> c) -> f a -> g b -> m (Info c)
liftBinOp f in1 in2 = do
  content in1 >>= \case
    Info x ->
      content in2 >>= \case
        Info y -> pure . Info $ f x y
        _ -> pure NoInfo
    _ -> pure NoInfo
