{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Eval.Stages where

import Data.Functor.Const
import Control.Monad.ST (ST)
import Data.Functor.Foldable (Fix(..))
import Data.STRef (STRef)
import Data.Sum
import Numeric.Natural

import AST.Core

-- | A polarized application.
--
-- Carries info about how to
-- evaluate input
data PApp a = PApp Polarity a a
deriving instance Functor PApp

papp :: (PApp :< fs) => Polarity -> Fix (Sum fs) -> Fix (Sum fs) -> Fix (Sum fs)
papp p func = Fix . inject . PApp p func

-- | The runtime stack, holding pointers to closures
newtype Stack a = Stack [a]
deriving instance Functor Stack

-- | A closure is either a suspended computation, a thunk,
-- or it's an expression in at least WHNF 
data Thunk b a = Thunk (Stack b) a
deriving instance Functor (Thunk b)

thunk :: (Thunk b :< fs)
  => Stack b 
  -> Fix (Sum fs)
  -> Fix (Sum fs)
thunk st = Fix . inject . Thunk st

-- | All symbols at this point are bound within the stack
-- and have been converted to pointers
--
-- Bound variables will *always* point to a closure of type a
-- but point to Any internally to allow for pointer reuse
-- through different phases
newtype Bound a = Bound Natural
deriving instance Functor Bound

bnd :: (Bound :< fs) => Natural -> Fix (Sum fs)
bnd = Fix . inject . Bound

-- | Expressions after type checking
-- ready to be evaluated
type RunTerm = Sum [Prim, Data, Bound, PApp] 

-- | Expressions during and after eval
newtype WHNF a = WHNF { unWHNF :: Sum [Prim, Data, Bound, Thunk (Fix WHNF), Norm] a }

type Norm = Const (Fix NF)  

-- | Fully evaluated term
type NF = Sum [Prim, Data] 
