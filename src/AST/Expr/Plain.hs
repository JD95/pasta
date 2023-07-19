{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Expr.Plain where

import AST.Expr (AST, Expr (..), ExprConfig (..), ExprF (..))
import AST.LocTree
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.Text
import GHC.Generics
import Parsing.Lexer (RowCol)

data Plain

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
