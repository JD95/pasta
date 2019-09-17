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
import           Numeric.Natural

import           Core
import           Display
import           Expr
import           Subst
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

cke :: Proxy Check
cke = Proxy @Check

hole :: String -> Fix CheckE
hole = Fix . inj . Hole

toCheck :: Fix CoreE -> Fix CheckE
toCheck = cata go
 where
  go (Here layer) = case layer of
    Val (Bound  i) -> mkVar cke i
    Val (Free   x) -> mkFree cke x
    Val (Inline x) -> mkInline cke x
    App x y        -> mkApp cke x y
    Lam x body     -> mkLam cke x body
  go (There (Here layer)) = case layer of
    RArr x y        -> mkRig cke x y
    PArr x y        -> mkPol cke x y
    TArr (a, b) x y -> mkArrow cke (Left a, Left b) x y
    TCon x          -> mkCon cke x
    Type n          -> mkT cke n
  go _ = undefined

instance Subst Checked Natural where
  depth f n = (,) n <$> f
  getKey = const Nothing
