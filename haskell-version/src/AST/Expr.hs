{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module AST.Expr where

import Data.Text

data Expr a
  = Lam a a
  | Ann a a
  | Let a a a
  | App a [a]
  | Symbol Text
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

display :: Expr Text -> Text
display (Lam input body) = "\\" <> input <> " -> " <> body
display (Symbol x) = x
display (Ann expr ty) = expr <> " : " <> ty
display (Let x y z) = "let " <> x <> " = " <> y <> " in " <> z
display (App f xs) = f <> " " <> intercalate " " xs
