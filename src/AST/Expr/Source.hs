{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Expr.Source where

import AST.Expr (AST, Expr (..), ExprConfig (..), ExprF (..))
import AST.Range
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import Data.Text
import GHC.Generics
import Lens.Micro.Platform
import Parsing.Lexer (RowCol)

data Src = Src Range
  deriving (Show, Eq, Ord)

instance HasRange Src where
  range = lens (\(Src r) -> r) (\(Src r) x -> Src x)

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
