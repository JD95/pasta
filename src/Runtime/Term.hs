module Runtime.Term where

import Control.Applicative
import Lattice
import Runtime.Prop
import Runtime.Ref
import System.Mem.StableName

data Term m = Term {unTerm :: Cell m (Ref m) (RtVal m)}

data RtVal m
  = RtList [RtVal m]
  | RtProdTy (RtVal m)
  | -- | Indexes
    --
    -- An index into a product type
    RtIndex Int (RtVal m)
  | RtSumTy (RtVal m)
  | RtCase Int (RtVal m)
  | -- | Lambdas
    --
    -- Since it doesn't make sense in general
    -- to check if functions are equal, we pair
    -- the generating code with a stable name to
    -- allow merging so long as the lambdas don't
    -- change
    --
    -- The closure here will maintain refs to
    -- previous cells used in the body, thus
    -- propagation will occur to those cells too
    RtLam (Stable (Term m -> m (Term m)))
  | -- | Holes are just unbound values
    Unbound

zipFail :: Alternative f => (a -> b -> f c) -> [a] -> [b] -> f [c]
zipFail _ [] [] = pure []
zipFail _ (_ : _) [] = empty
zipFail _ [] (_ : _) = empty
zipFail f (x : xs) (y : ys) =
  (:) <$> f x y <*> zipFail f xs ys

instance Lattice (RtVal m) where
  bottom = Unbound
  merge (Old old) (New new) =
    case (old, new) of
      (Unbound, Unbound) -> None
      (Unbound, other) -> Gain other
      --
      (RtLam (Stable x _), RtLam (Stable y _)) ->
        if x == y then None else Conflict
      (RtLam (Stable _ _), _) -> Conflict
      --
      (RtList xs, RtList ys) ->
        RtList <$> zipFail merge (Old <$> xs) (New <$> ys)
      (RtList _, _) -> Conflict
      --
      (RtProdTy x, RtProdTy y) -> merge (Old x) (New y)
      (RtProdTy _, _) -> Conflict
      --
      (RtIndex i x, RtIndex j y) ->
        if i == j then merge (Old x) (New y) else Conflict
      (RtIndex _ _, _) -> Conflict
      --
      (RtSumTy x, RtSumTy y) -> merge (Old x) (New y)
      (RtSumTy _, _) -> Conflict
      --
      (RtCase i x, RtCase j y) ->
        if i == j then merge (Old x) (New y) else Conflict
      (RtCase _ _, _) -> Conflict

  isTop _ = False

data Stable a = Stable (StableName a) a

instance Eq (Stable a) where
  (Stable x _) == (Stable y _) = x == y

data Strict a = Strict {force :: !a}
