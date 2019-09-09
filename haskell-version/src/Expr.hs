{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Expr where

import           Data.Functor.Const
import           Data.Functor.Foldable
import           Data.Functor.Identity
import           Data.Void
import           Numeric.Natural
import           Data.Proxy

import           Constructors
import           Subst
import           Summable

data Expr ix a where
  App :: a -> a -> Expr ix a
  Lam :: LamOpts ix -> a -> Expr ix a
  Val :: Abst a -> Expr ix a

deriving instance Functor (Expr ix)

class Expression ix where
  type LamOpts ix :: *

data Abst a = Inline a | Bound Natural | Free String deriving (Show, Functor)

data PrintExpr ix
  = MkPrintExpr
  { printLamOpts :: LamOpts ix -> String -> String
  }

mkApp
  :: (Injectable (Expr ix) xs)
  => Proxy ix
  -> Fix (Summed xs)
  -> Fix (Summed xs)
  -> Fix (Summed xs)
mkApp = \(_ :: Proxy ix) func input -> Fix . inj $ App @_ @ix func input

mkLam
  :: (Injectable (Expr ix) xs)
  => Proxy ix
  -> LamOpts ix
  -> Fix (Summed xs)
  -> Fix (Summed xs)
mkLam = \(_ :: Proxy ix) opts body -> Fix . inj $ Lam @ix opts body

mkVar :: (Injectable (Expr ix) xs) => Proxy ix -> Natural -> Fix (Summed xs)
mkVar = \(_ :: Proxy ix) i -> Fix . inj . Val @_ @ix $ Bound i

mkFree :: (Injectable (Expr ix) xs) => Proxy ix -> String -> Fix (Summed xs)
mkFree = \(_ :: Proxy ix) name -> Fix . inj . Val @_ @ix $ Free name

mkInline
  :: (Injectable (Expr ix) xs) => Proxy ix -> Fix (Summed xs) -> Fix (Summed xs)
mkInline = \(_ :: Proxy ix) x -> Fix . inj . Val @_ @ix $ Inline x

printExpr :: PrintExpr ix -> Expr ix String -> String
printExpr (MkPrintExpr lam) = go
 where
  go (App func input) = func <> " " <> input
  go (Lam name body ) = concat ["(\\", lam name body, " -> ", body, ")"]
  go (Val x         ) = printAbst id x

printAbst :: (a -> String) -> Abst a -> String
printAbst f (Inline x) = f x
printAbst _ (Bound  i) = "%" <> show i
printAbst _ (Free   x) = x

instance Subst (Expr ix) Natural where
  depth (Lam _ _) n = n + 1
  depth _ n = n

  getKey (Val (Bound i)) = Just i
  getKey _ = Nothing
