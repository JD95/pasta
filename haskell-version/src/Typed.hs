{-# LANGUAGE FlexibleInstances, FlexibleContexts, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Typed where

import           Data.Functor.Foldable
import           Data.Functor.Const
import           Data.Void
import           Numeric.Natural
import           Data.Proxy

import           Constructors
import           Summable
import           Subst

class TypedExpression ix where
  type RigName ix :: *
  type PolName ix :: *
  type ArrowOpts ix :: *

data Typed ix a where
  RArr :: RigName ix -> a -> Typed ix a
  PArr :: PolName ix -> a -> Typed ix a
  TArr :: ArrowOpts ix -> a -> a -> Typed ix a
  TCon :: String -> Typed ix a
  Type :: Natural -> Typed ix a

deriving instance Functor (Typed ix)

data Pol = S | L deriving (Show, Eq)
data Rig = R0 | R1 | RU deriving (Show, Eq)

instance Semigroup Rig where
  R0 <> x  = x
  x  <> R0 = x
  R1 <> R1 = RU
  RU <> R1 = RU
  _  <> RU = RU

instance Monoid Rig where
  mempty = R0

printPol :: Pol -> String
printPol S = "!"
printPol L = "?"

printRig :: Rig -> String
printRig R0 = "0"
printRig R1 = "1"
printRig RU = ""

data PrintTyped ix
  = MkPrintTyped
  { printRigName :: RigName ix -> String
  , printPolName :: PolName ix -> String
  , printArrowOpts :: ArrowOpts ix -> String -> String -> String
  }

printTyped :: PrintTyped ix -> Typed ix String -> String
printTyped (MkPrintTyped r p arr) = go
 where
  go (RArr name output      ) = concat ["[", r name, " : Rig] -> ", output]
  go (PArr name output      ) = concat ["[", p name, " : Pol] -> ", output]
  go (TArr opts input output) = concat [arr opts input output, " -> ", output]
  go (TCon name             ) = name
  go (Type n                ) = "Type " <> show n

data TypeBuilder ix xs
  = TypeBuilder
  { mkRig :: RigName ix -> Fix (Summed xs) -> Fix (Summed xs)
  , mkPol :: PolName ix -> Fix (Summed xs) -> Fix (Summed xs)
  , mkArrow :: ArrowOpts ix -> Fix (Summed xs) -> Fix (Summed xs) -> Fix (Summed xs)
  , mkCon :: String -> Fix (Summed xs)
  , mkT :: Natural -> Fix (Summed xs)
  }

typeBuilder
  :: (TypedExpression ix, (Typed ix :<: xs)) => Proxy ix -> TypeBuilder ix xs
typeBuilder (_ :: Proxy ix) = TypeBuilder
  { mkRig   = \name output -> Fix $ inj $ RArr @ix name output
  , mkPol   = \name output -> Fix . inj $ PArr @ix name output
  , mkArrow = \opts input output -> Fix . inj $ TArr @ix opts input output
  , mkCon   = \name -> Fix . inj $ TCon @ix name
  , mkT     = \n -> Fix . inj $ Type @ix n
  }

instance Subst (Typed ix) Natural where
  depth (RArr _ _) n = n + 1
  depth (PArr _ _) n = n + 1
  depth (TArr _ _ _) n = n + 1
  depth _ n = n

  getKey _ = Nothing
