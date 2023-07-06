{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Expr where

import Data.Functor.Foldable.TH (makeBaseFunctor)

data Expr l h r
  = Lam l (Expr l h r)
  | Ann (Expr l h r) (Expr l h r)
  | Let (Expr l h r) (Expr l h r) (Expr l h r)
  | App (Expr l h r) [(Expr l h r)]
  | Arr (Maybe r) (Expr l h r) (Expr l h r)
  | Symbol r
  | Hole h
  | Prod [(Expr l h r)]
  deriving (Show, Eq)

makeBaseFunctor ''Expr
