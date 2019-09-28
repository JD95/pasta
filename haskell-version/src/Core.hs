{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Core where

import           Data.Functor.Foldable
import           Data.Proxy

import           Expr
import           Typed
import           Display
import           Summable

data Core

instance Expression Core where
  type LamOpts Core = ()
  type CaseOpts Core = ()

instance TypedExpression Core where
  type RigName Core = ()
  type PolName Core = ()
  type ArrowOpts Core = (Abst Rig, Abst Pol)

type CoreE = Summed '[ Expr Core, Typed Core ]

instance Display (Expr Core String) where
  display = printExpr $ MkPrintExpr
    { printLamOpts   = \_ _ -> "_"
    , printCaseOpts = const "_"
    }

instance Display (Typed Core String) where
  display =  printTyped $ MkPrintTyped
      { printRigName    = const ""
      , printPolName    = const ""
      , printArrowOpts  = \(rig, inPol) input _ -> concat
        [ "("
        , printAbst printRig rig
        , printAbst printPol inPol
        , " "
        , input
        , ")"
        ]
      }

class ToCore f where
  toCore :: Fix f -> Fix CoreE

ce :: Proxy Core
ce = Proxy @Core
