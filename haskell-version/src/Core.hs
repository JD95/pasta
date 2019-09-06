{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Core where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Control.Arrow           hiding ( app )
import           Control.Comonad.Trans.Cofree   ( CofreeF(..) )
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Functor.Const
import           Data.Functor.Foldable
import qualified Data.Map.Strict               as Map
import           Data.Void
import           Numeric.Natural

import           Env
import           Expr
import           Typed
import           Subst
import           Display
import           Summable

data Core

instance Expression Core where
  type LamOpts Core = ()

instance TypedExpression Core where
  type RigName Core = ()
  type PolName Core = ()
  type ArrowOpts Core = (Abst Rig, Abst Pol, Abst Pol)

type CoreE = Summed '[ Expr Core, Typed Core ]

instance Display (Expr Core String) where
  display = printExpr $ MkPrintExpr { printLamOpts   = \_ _ -> "_" }

instance Display (Typed Core String) where
  display =  printTyped $ MkPrintTyped
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
      }

class ToCore f where
  toCore :: Fix f -> Fix CoreE
