{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Control.Arrow hiding (app)
import           Control.Comonad.Trans.Cofree (CofreeF(..))
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Functor.Const
import           Data.Functor.Foldable
import qualified Data.Map.Strict as Map
import           Data.Void
import           Numeric.Natural

import           Env
import           Expr
import           Typed

data Core

instance Expression Core where
  type LamOpts Core = ()
  type ExprExt Core f = Typed Core f

instance TypedExpression Core where
  type RigName Core = ()
  type PolName Core = ()
  type ArrowOpts Core = (Abst Rig, Abst Pol, Abst Pol)
  type TypedExt Core f = Const Void

newtype CoreE a
  = MkCoreE
  { unCoreE :: Expr Core (Typed Core (Const Void)) a
  } deriving (Functor)

instance ExprConst Core CoreE where
  injExpr = MkCoreE

instance TypedConst Core CoreE where
  injTyped = MkCoreE . Expr

printCore :: CoreE String -> String
printCore = go . unCoreE
 where
  go = printExpr $ MkPrintExpr
    { printLamOpts   = \_ _ -> "_"
    , printExprInner = printTyped $ MkPrintTyped
      { printRigName    = const ""
      , printPolName    = const ""
      , printArrowOpts  = \(rig, inPol, outPol) input _ -> concat
        [ "["
        , input
        , ", "
        , printAbst printRig rig
        , ", "
        , printAbst printPol inPol
        , ", "
        , printAbst printPol outPol
        , "]"
        ]
      , printTypedInner = const ""
      }
    }
