{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Expr.Source where

import AST.Expr (AST, Expr (..), ExprConfig (..), ExprF (..))
import AST.LocTree
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.Text
import GHC.Generics
import Parsing.Lexer (RowCol)

data Src

instance ExprConfig Src where
  type LamTy Src = Text
  type BranchTy Src = AST Src
  type HoleTy Src = Text
  type RefTy Src = Text

deriving instance Generic1 (ExprF Src)

deriving via FunctorClassesDefault (ExprF Src) instance Eq1 (ExprF Src)

deriving via FunctorClassesDefault (ExprF Src) instance Show1 (ExprF Src)

deriving instance Eq (Expr Src)

deriving instance Show (Expr Src)
