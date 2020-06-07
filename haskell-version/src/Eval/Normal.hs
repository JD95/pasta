{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Eval.Normal where

import Control.Monad.ST (ST)
import Data.Functor.Const
import Data.Functor.Foldable (Fix(..), unfix, cata)
import Data.STRef (readSTRef, writeSTRef)
import Data.Sum
import Prelude hiding (lookup)

import AST.Core
import AST.Transform
import Eval.Stages

-- -----------------------------------------------------------
-- |
-- * Evaluates WHNF terms into NF, removing
--   all closures.
class Normal f where
  normal :: f (Stack (Fix NF) -> Fix NF) -> (Stack (Fix NF) -> Fix NF)

instance Apply Normal fs => Normal (Sum fs) where
  normal = apply @Normal normal

instance Normal Prim where
  normal = pass

instance Normal Data where
  normal = pass

instance Normal Bound where
  normal (Bound ref) (Stack st) = st !! fromIntegral ref

instance Normal (Thunk (Fix NF)) where
  normal (Thunk st comp) _ = comp st

instance Normal Norm where
  normal = pure . getConst
