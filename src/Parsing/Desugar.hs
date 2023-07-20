{-# LANGUAGE LambdaCase #-}

module Parsing.Desugar where

import AST.Expr
import AST.Expr.Plain
import AST.Expr.Source
import AST.LocTree
import Data.Functor.Foldable

desugar :: AST Src -> AST Plain
desugar = futu $ \case
  LocTree p q val -> case val of
    HoleF t -> LocTreeF p q $ HoleF t
    ProdF xs -> LocTreeF p q $ ProdF (pure <$> xs)
    LamF _ _ -> undefined
    AnnF _ _ -> undefined
    LetF _ _ _ -> undefined
    AppF _ _ -> undefined
    CaseOfF _ _ -> undefined
    ArrF _ _ _ -> undefined
    SymbolF _ -> undefined
