{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Expr where

import           Data.Functor.Const
import           Data.Functor.Foldable
import           Data.Functor.Identity
import           Data.Void
import           Numeric.Natural

import           Constructors
import           Subst

data Expr ix f a where
  App :: a -> a -> Expr ix f a
  Lam :: LamOpts ix -> a -> Expr ix f a
  Val :: Abst a -> Expr ix f a
  Expr :: f a -> Expr ix f a

deriving instance (Functor f) => Functor (Expr ix f)

class Expression ix where
  type LamOpts ix :: *
  type ExprExt ix (f :: * -> *) :: * -> *

data Abst a = Inline a | Bound Natural | Free String deriving (Show, Functor)

data PrintExpr ix f
  = MkPrintExpr
  { printLamOpts :: LamOpts ix -> String -> String
  , printExprInner :: f String -> String
  }

printExpr :: PrintExpr ix f -> Expr ix f String -> String
printExpr (MkPrintExpr lam inner) = go
 where
  go (App func input) = func <> " " <> input
  go (Lam name body ) = concat ["(\\", lam name body, " -> ", body, ")"]
  go (Val  x        ) = printAbst id x
  go (Expr x        ) = inner x

printAbst :: (a -> String) -> Abst a -> String
printAbst f (Inline x) = f x
printAbst _ (Bound  i) = "%" <> show i
printAbst _ (Free   x) = x

class (Functor f, Expression ix) => ExprConst ix f g | ix -> f, g -> ix where
  injExpr :: Expr ix (ExprExt ix f) a -> g a

app func input = Fix . injExpr $ App func input

lam opts body = Fix . injExpr $ Lam opts body

var i = Fix . injExpr . Val $ Bound i

free name = Fix . injExpr . Val $ Free name

inline x = Fix . injExpr . Val . Inline $ x
