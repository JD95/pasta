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

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Kind
import Data.Text

data ExprConfig
  = ExprConfig
  -- | Lambda Type
  Type
  -- | Hole Type
  Type
  -- | Ref Type
  Type

type family LamTy (i :: ExprConfig) :: Type where
  LamTy ('ExprConfig ty _ _) = ty

type family HoleTy (i :: ExprConfig) :: Type where
  HoleTy ('ExprConfig _ ty _) = ty

type family RefTy (i :: ExprConfig) :: Type where
  RefTy ('ExprConfig _ _ ty) = ty

data Expr (c :: ExprConfig)
  = Lam (LamTy c) (Expr c)
  | Ann (Expr c) (Expr c)
  | Let (Expr c) (Expr c) (Expr c)
  | App (Expr c) [Expr c]
  | Arr (Maybe (RefTy c)) (Expr c) (Expr c)
  | Symbol (RefTy c)
  | Hole (HoleTy c)
  | Prod [Expr c]

makeBaseFunctor ''Expr

type Src = 'ExprConfig Text Text Text

deriving instance Eq (Expr Src)
