{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Typed where

import           Data.Functor.Foldable
import           Data.Functor.Const
import           Data.Void
import           Numeric.Natural

import           Constructors

class TypedExpression ix where
  type RigName ix :: *
  type PolName ix :: *
  type ArrowOpts ix :: *
  type TypedExt ix (f :: * -> *) :: * -> *

data Typed ix f a where
  RArr :: RigName ix -> a -> Typed ix f a
  PArr :: PolName ix -> a -> Typed ix f a
  TArr :: ArrowOpts ix -> a -> a -> Typed ix f a
  TCon :: String -> Typed ix f a
  Type :: Natural -> Typed ix f a
  Typed :: f a -> Typed ix f a

deriving instance (Functor f) => Functor (Typed ix f)

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

data PrintTyped ix f
  = MkPrintTyped
  { printRigName :: RigName ix -> String
  , printPolName :: PolName ix -> String
  , printArrowOpts :: ArrowOpts ix -> String -> String -> String
  , printTypedInner :: f String -> String
  }

printTyped :: PrintTyped ix f -> Typed ix f String -> String
printTyped (MkPrintTyped r p arr inner) = go
 where
  go (RArr name output      ) = concat ["[", r name, " : Rig] -> ", output]
  go (PArr name output      ) = concat ["[", p name, " : Pol] -> ", output]
  go (TArr opts input output) = concat [arr opts input output, " -> ", output]
  go (TCon  name            ) = name
  go (Type  n               ) = "Type " <> show n
  go (Typed x               ) = inner x

class TypedExpression ix => TypedConst ix f g | ix -> f, g -> ix where
  injTyped :: Typed ix (TypedExt ix f) a -> g a

rig name output = Fix . injTyped $ RArr name output

pol name output = Fix . injTyped $ PArr name output

arrow opts input output = Fix . injTyped $ TArr opts input output

con name = Fix . injTyped $ TCon name

t_ n = Fix . injTyped $ Type n

