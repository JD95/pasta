{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Eval.WHNF where

import           Control.Monad.ST (ST)
import           Data.Functor.Foldable (Fix(..))
import           Data.Sum

import Eval.Stages

class Run s f where
  run :: f (Fix (RunTerm s)) -> ST s (Fix (WHNF s))
  
instance Apply (Run s) fs => Run s (Sum fs) where
  run = apply @(Run s) run 

