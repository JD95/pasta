module Parsing.Desugar where

import AST.Expr
import AST.Expr.Plain
import AST.Expr.Source
import Control.Monad.Free
import Data.Functor.Foldable
import Parsing.Lexer (RowCol)

desugar :: AST Src -> AST Plain
desugar = futu undefined
