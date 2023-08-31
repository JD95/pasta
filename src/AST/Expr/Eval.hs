module Expr.Eval where

import AST.Expr
import Data.Map
import Data.Text
import Data.Void
import Runtime.Term

data Flow
  = Unkown
  | In
  | Out
  | Total

data Wiring k m = Wiring
  {wiring :: Map k (Flow, Term m)}

data Runtime (m :: Type -> Type)
  = Refined

instance ExprConfig (Runtime m) where
  type LamTy (Runtime m) = Void
  type BranchTy (Runtime m) = Int
  type HoleTy (Runtime m) = Term m
  type RefTy (Runtime m) = Term m
  type RelTy (Runtime m) = Wiring Text m
