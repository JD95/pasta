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
import           Expr
import           Typed
import           Summable

data Checked a = Hole String deriving (Functor)

data TypeCheckError

type CheckE = Summed '[ Expr Check, Typed Check, Checked]

data Check

instance Expression Check where
  type LamOpts Check = LamOpts Core

instance TypedExpression Check where
  type RigName Check = RigName Core
  type PolName Check = RigName Core
  type ArrowOpts Check = ( Either (Abst Rig) (Fix CheckE)
                         , Either (Abst Pol) (Fix CheckE)
                         , Either (Abst Pol) (Fix CheckE)
                         )

cke = Proxy @Check

hole :: String -> Fix CheckE
hole = Fix . inj . Hole
