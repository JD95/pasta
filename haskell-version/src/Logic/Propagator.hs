{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logic.Propagator () where

import Control.Monad.Fail
import Control.Monad.IO.Class
import Data.List.NonEmpty
import Logic.Info
import Logic.Propagator.Class
import Logic.Propagator.IOCell

test :: (m ~ IO, MonadFail m, MonadIO m, Network m) => m (Info Double)
test = do
  x <- newIOCell
  y <- newIOCell
  z <- newIOCell
  adder x y z
  inform (Info 1.0) x
  inform (Info 5.0) z
  solve
  content y

adder :: (Merge a, Num a, MonadFail m, Network m, Cell m f, Cell m g, Cell m h) => f a -> g a -> h a -> m ()
adder in1 in2 out = do
  watchCells (SomeCell in1 :| [SomeCell in2]) $ propagate (add in1 in2) [out]
  watchCells (SomeCell out :| [SomeCell in2]) $ propagate (sub out in2) [in1]
  watchCells (SomeCell out :| [SomeCell in1]) $ propagate (sub out in1) [in2]

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
