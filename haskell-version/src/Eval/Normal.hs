{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Eval.Normal where

import Control.Monad.ST (ST)
import Data.Functor.Foldable (Fix(..), unfix)
import Data.STRef (readSTRef, writeSTRef)
import Data.Sum

import AST.Core
import AST.Transform
import Eval.Stages

-- -----------------------------------------------------------
-- |
-- * Evaluates WHNF terms into NF, removing
--   all closures.
class Normal s f where
  normal :: f (Fix (WHNF s)) -> ST s (Fix NF)

instance Apply (Normal s) fs => Normal s (Sum fs) where
  normal = apply @(Normal s) normal

instance Normal s Prim where
  normal = pass . fmap (normal @s . unfix) 

instance Normal s Data where
  normal = pass . fmap (normal @s . unfix) 

instance Normal s (Bound s (Fix NF)) where
  normal (Bound ref) = do
    value <- readSTRef ref
    result <- normal value
    writeSTRef ref . Clo . Left $ result
    pure result

instance Normal s (Closure s (Fix NF)) where
  normal (Clo value) =
    case value of
      Right (comp, st) -> normal . unfix =<< (comp st)
      Left x -> pure x

