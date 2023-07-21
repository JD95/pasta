{-# LANGUAGE LambdaCase #-}

module Parsing.Desugar where

import AST.Expr
import AST.Expr.Plain
import AST.Expr.Source
import AST.Range
import AST.Tree
import Data.Functor.Foldable

desugar :: AST Src -> AST Plain
desugar = futu $ \case
  Tree (Src (Range p q)) val -> case val of
    HoleF t -> TreeF (Plain (Range p q)) $ HoleF t
    ProdF xs -> TreeF (Plain (Range p q)) $ ProdF (pure <$> xs)
    LamF _ _ -> undefined
    AnnF _ _ -> undefined
    LetF _ _ _ -> undefined
    AppF _ _ -> undefined
    CaseOfF _ _ -> undefined
    ArrF _ _ _ -> undefined
    SymbolF _ -> undefined
