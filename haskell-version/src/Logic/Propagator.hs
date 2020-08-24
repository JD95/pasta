{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logic.Propagator () where

import Control.Monad.Fail
import Control.Monad.IO.Class
import Logic.Info
import Logic.Propagator.Class
import Logic.Propagator.IOCell

test :: (MonadFail m, MonadIO m, Network m) => m ()
test = do
  x <- newIOCell
  y <- newIOCell
  z <- newIOCell
  adder x y z
  solve

adder :: (MonadFail m, Network m, Cell m f, Cell m g, Cell m h) => f Int -> g Int -> h Int -> m ()
adder in1 in2 out = do
  propagate (add in1 in2) [out]
  propagate (sub out in2) [in1]
  propagate (sub out in1) [in2]

add :: (Num a, MonadFail m, Cell m f, Cell m g) => f a -> g a -> m (Info a)
add = liftBinOp (+)

sub :: (Num a, MonadFail m, Cell m f, Cell m g) => f a -> g a -> m (Info a)
sub = liftBinOp (-)

liftBinOp :: (MonadFail m, Cell m f, Cell m g) => (a -> b -> c) -> f a -> g b -> m (Info c)
liftBinOp f in1 in2 = do
  Info x <- content in1
  Info y <- content in2
  pure . Info $ f x y
