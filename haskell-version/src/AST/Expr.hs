{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Expr where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text

data Expr
  = Lam Expr Expr
  | Ann Expr Expr
  | Let Expr Expr Expr
  | App Expr [Expr]
  | Symbol Text
  | Prod [Expr]
  deriving (Show, Eq)

makeBaseFunctor ''Expr

display :: ExprF Text -> Text
display (LamF input body) = "\\" <> input <> " -> " <> body
display (SymbolF x) = x
display (AnnF expr ty) = expr <> " : " <> ty
display (LetF x y z) = "let " <> x <> " = " <> y <> " in " <> z
display (AppF f xs) = f <> " " <> intercalate " " xs
display (ProdF xs) = "(" <> intercalate ", " xs <> ")"
