{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Eval.Stages where

import Control.Monad.ST (ST)
import Data.Functor.Foldable (Fix(..))
import Data.STRef (STRef)
import Data.Sum

import AST.Core

-- | A Bound Lambda term.
--
-- Carries the ref to the bound value in the
-- body.
data BLam s b a = BLam (STRef s (Closure s b a)) a

blam :: (BLam s b :< fs) => STRef s (Closure s b (Fix (Sum fs))) -> Fix (Sum fs) -> Fix (Sum fs)
blam ref = Fix . inject . BLam ref

-- | A polarized application.
--
-- Carries info about how to
-- evaluate input
data PApp a = PApp Polarity a a
deriving instance Functor PApp

papp :: (PApp :< fs) => Polarity -> Fix (Sum fs) -> Fix (Sum fs) -> Fix (Sum fs)
papp p func = Fix . inject . PApp p func

-- | The runtime stack, holding pointers to closures
newtype Stack s b a = Stack [STRef s (Closure s b a)]

-- | A closure is either a suspended computation, a thunk,
-- or it's an expression in at least WHNF 
newtype Closure s b a = Clo (Either b (Stack s b a -> ST s a, Stack s b a))

thunk :: (Closure s b :< fs)
  => Stack s b (Fix (Sum fs))
  -> (Stack s b (Fix (Sum fs))
  -> ST s (Fix (Sum fs)))
  -> Fix (Sum fs)
thunk st comp = Fix . inject . Clo . Right $ (comp, st)

newtype Val s a = V a

val :: (Closure s b :< fs) => Val s b -> Fix (Sum fs)
val (V x :: Val s b) = Fix . inject @(Closure s b) . Clo . Left $ x

-- | All symbols at this point are bound within the stack
-- and have been converted to pointers
--
-- Bound variables will *always* point to a closure of type a
-- but point to Any internally to allow for pointer reuse
-- through different phases
newtype Bound s b a = Bound (STRef s (Closure s b a)) 

bnd :: (Bound s b :< fs) => STRef s (Closure s b (Fix (Sum fs))) -> Fix (Sum fs)
bnd = Fix . inject . Bound

-- | Expressions after type checking
-- ready to be evaluated
type RunTerm s = Sum [Prim, Data, Bound s (Fix (WHNF s)), PApp, BLam s (Fix (WHNF s))]

-- | Expressions during and after eval
type WHNF s = Sum [Prim, Data, Bound s (Fix NF), Closure s (Fix NF)]

-- | Fully evaluated term
type NF = Sum [Prim, Data]
