{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Expr.Refined where

import AST.Expr
import AST.Range
import AST.Tree
import Data.Kind
import Lens.Micro.Platform
import Runtime.Term

data Refined (m :: Type -> Type)
  = Refined Range (Term m)

class HasType m a where
  typeOf :: Lens' a (Term m)

instance HasType m (Refined m) where
  typeOf = lens (\(Refined _ ty) -> ty) (\(Refined r _) ty -> Refined r ty)

instance HasType m t => HasType m (Tree t f) where
  typeOf = lens (view (ctx . typeOf)) (flip $ set (ctx . typeOf))

instance ExprConfig (Refined m) where
  type LamTy (Refined m) = Term m
  type BranchTy (Refined m) = Int
  type HoleTy (Refined m) = Term m
  type RefTy (Refined m) = Term m
