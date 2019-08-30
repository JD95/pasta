{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core where

import           Numeric.Natural
import           Data.Functor.Const
import           Data.Void

import           Expr
import           Typed

data Core

instance Expression Core where
  type LamOpts Core = Natural
  type ExprExt Core f = Typed Core f

instance TypedExpression Core where
  type ArrowOpts Core = (Natural, Abst Rig, Abst Pol, Abst Pol)
  type TypedExt Core f = Const Void

newtype CoreE a = MkCoreE { unCoreE :: Expr Core (Typed Core (Const Void)) a }

instance TypedConst Core CoreE where
  injTyped = MkCoreE . Expr
