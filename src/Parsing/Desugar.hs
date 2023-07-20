module Parsing.Desugar where

import AST.Expr
import AST.Expr.Plain
import AST.Expr.Source
import Data.Functor.Foldable

desugar :: AST Src -> AST Plain
desugar = futu undefined
