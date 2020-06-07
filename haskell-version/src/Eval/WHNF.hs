{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Eval.WHNF where

import Control.Monad.ST (ST)
import Data.Functor.Foldable (Fix(..))
import Data.Sum

import AST.Core
import AST.Transform
import Eval.Stages

class Run f where
  run :: f (Stack (Fix WHNF) -> Fix WHNF) -> (Stack (Fix WHNF) -> Fix WHNF) 
  
instance Apply Run fs => Run (Sum fs) where
  run = apply @Run run 

instance Run Prim where
  run = gpass WHNF

instance Run Data where
  run = gpass WHNF

instance Run Bound where
  run (Bound ref) (Stack st) = st !! fromIntegral ref

instance Run PApp where 
  run (PApp Deep _ _) = error "Deep Eval not supported"
  run (PApp Shallow _ _) = error "Shallow Eval not supported"
  run (PApp Lazy _ _) = error "Lazy Eval not supported"
  run (PApp Logic _ _) = error "Logic Eval not supported"


