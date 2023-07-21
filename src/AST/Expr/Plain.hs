{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Expr.Plain where

import AST.Expr (AST, Expr (..), ExprConfig (..), ExprF (..))
import AST.Range
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.Text
import GHC.Generics
import Lens.Micro.Platform
import Parsing.Lexer (RowCol)

data Plain = Plain Range
  deriving (Eq, Ord, Show)

instance HasRange Plain where
  range = lens (\(Plain r) -> r) (\(Plain r) x -> Plain x)

instance ExprConfig Plain where
  type LamTy Plain = Text
  type BranchTy Plain = AST Plain
  type HoleTy Plain = Text
  type RefTy Plain = Text

deriving instance Generic1 (ExprF Plain)

deriving via FunctorClassesDefault (ExprF Plain) instance Eq1 (ExprF Plain)

deriving via FunctorClassesDefault (ExprF Plain) instance Show1 (ExprF Plain)

deriving instance Eq (Expr Plain)

deriving instance Show (Expr Plain)
