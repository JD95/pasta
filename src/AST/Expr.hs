{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Expr where

import AST.LocTree
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Kind
import Data.Text
import Lexer (RowCol)

class ExprConfig c where
  type LamTy c :: Type
  type BranchTy c :: Type
  type HoleTy c :: Type
  type RefTy c :: Type

data Expr c
  = Lam (LamTy c) (Expr c)
  | Ann (Expr c) (Expr c)
  | Let (Expr c) (Expr c) (Expr c)
  | App (Expr c) [Expr c]
  | CaseOf (Expr c) [(BranchTy c, Expr c)]
  | Arr (Maybe (RefTy c)) (Expr c) (Expr c)
  | Symbol (RefTy c)
  | Hole (HoleTy c)
  | Prod [Expr c]

makeBaseFunctor ''Expr

type AST x = LocTree RowCol (ExprF x)
