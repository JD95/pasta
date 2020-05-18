{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}

module TypeCheck.Typed where

import           Data.Sum

import AST.Core

data Ann a where
  Ann :: a -> a -> Ann a 
deriving instance Functor Ann
deriving instance Foldable Ann
deriving instance Traversable Ann

-- | Expressions with Types and free variables 
type TypedTerm = Sum [Prim, Data, App, Lam, FreeVar, Ann]
