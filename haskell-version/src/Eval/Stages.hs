{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Eval.Stages where

import AST.Core
import Data.Functor.Const
import Data.Functor.Foldable (Fix (..), unfix)
import Data.IORef (IORef)
import Data.Sum
import Numeric.Natural

-- | A polarized application.
--
-- Carries info about how to
-- evaluate input
data PApp a = PApp Polarity a a

deriving instance Functor PApp

papp :: (PApp :< fs) => Polarity -> Fix (Sum fs) -> Fix (Sum fs) -> Fix (Sum fs)
papp p func = Fix . inject . PApp p func

newtype Stack a = Stack [IORef a]

-- | The runtime stack, holding pointers to closures
data Thunk b a = Thunk (Stack b) a

deriving instance Functor (Thunk b)

thunk :: (Thunk b :< fs) => Stack b -> Fix (Sum fs) -> Fix (Sum fs)
thunk st = Fix . inject . Thunk st

newtype Bound a = Bound Natural

deriving instance Functor Bound

bnd :: (Bound :< fs) => Natural -> Fix (Sum fs)
bnd = Fix . inject . Bound

type TermComps b = Sum [Prim, Data, Thunk b, Bound, Norm]

-- | Expressions during and after eval
newtype Term a = Term {unTerm :: TermComps (Fix Term) a}

term :: (fs ~ TermComps (Fix Term)) => Fix fs -> Fix Term
term = Fix . Term . fmap term . unfix

deriving instance Functor Term

type Norm = Const (Fix NF)

-- | Fully evaluated term
type NF = Sum '[Prim, Data]
