{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Eval.Stages where

import           Control.Monad.ST (ST)
import           Data.Functor.Foldable (Fix(..))
import           Data.STRef (STRef)
import           Data.Sum

import AST.Core

-- | A Bound Lambda term.
--
-- Carries the ref to the bound value in the
-- body.
data BLam s b a = BLam (STRef s (Closure s b a)) a

-- | A polarized application.
--
-- Carries info about how to
-- evaluate input
data PApp a = PApp Polarity a a
deriving instance Functor PApp

-- | The runtime stack, holding pointers to closures
newtype Stack s b a = Stack [STRef s (Closure s b a)]

-- | A closure is either a suspended computation, a thunk,
-- or it's an expression in at least WHNF 
newtype Closure s b a = Clo (Either b (Stack s b a -> ST s a, Stack s b a))

-- | All symbols at this point are bound within the stack
-- and have been converted to pointers
--
-- Bound variables will *always* point to a closure of type a
-- but point to Any internally to allow for pointer reuse
-- through different phases
newtype Bound s b a = Bound (STRef s (Closure s b a)) 

-- | Expressions after type checking
-- ready to be evaluated
type RunTerm s = Sum [Prim, Data, Bound s (Fix (WHNF s)), PApp, BLam s (Fix (WHNF s))]

-- | Expressions during and after eval
type WHNF s = Sum [Prim, Data, Bound s (Fix NF), Closure s (Fix NF)]

-- | Fully evaluated term
type NF = Sum [Prim, Data]
