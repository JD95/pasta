{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.TypeCheck.Check where

import           Data.Functor.Foldable
import           Data.Proxy

import           Core
import           Display
import           Expr
import           Summable
import           Typed

data Checked a = Hole String deriving (Functor)

instance (Display a) => Display (Checked a) where
  display (Hole name) = "?" <> name

data TypeCheckError

type CheckE = Summed '[ Expr Check, Typed Check, Checked]

instance Display (Expr Check String) where
  display = printExpr $ MkPrintExpr { printLamOpts = (const . const) "_" }

instance Display (Typed Check String) where
  display = printTyped $ MkPrintTyped
    { printRigName = const ""
    , printPolName = const ""
    , printArrowOpts  = \(rig, inPol) input _ -> concat
      [ "("
      , either (printAbst printRig) (cata display) rig
      , " "
      , either (printAbst printPol) (cata display) inPol
      , " : "
      , input
      , ")"
      ]
    }

data Check

instance Expression Check where
  type LamOpts Check = LamOpts Core

instance TypedExpression Check where
  type RigName Check = RigName Core
  type PolName Check = RigName Core
  type ArrowOpts Check = ( Either (Abst Rig) (Fix CheckE)
                         , Either (Abst Pol) (Fix CheckE)
                         )

cke = Proxy @Check

hole :: String -> Fix CheckE
hole = Fix . inj . Hole