{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Refinement (Env (..), refinement, realization) where

import AST.Expr
import AST.Expr.Plain
import AST.LocTree
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Logic
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.IORef
import Data.Kind
import Lattice
import Runtime.Prop
import Runtime.Ref
import System.Mem.StableName

class
  ( MonadReader (Env m) m,
    Alternative m,
    MonadRef m,
    Monad m
  ) =>
  MonadRefine m

newtype Refine a = Refine {runRefine :: LogicT (ReaderT (Env Refine) IO) a}
  deriving (Functor, Applicative, Monad, MonadReader (Env Refine), Alternative, MonadIO)

instance MonadRef Refine where
  type Ref Refine = IORef
  newRef = liftIO . newIORef
  readRef = liftIO . readRef
  writeRef r = liftIO . writeIORef r

instance MonadRefine Refine

data RefineError = RefineError

refinement :: AST Plain -> Env Refine -> IO (Either RefineError (AST (Rt Refine)))
refinement input env = do
  runReaderT (runLogicT (runRefine $ refine input) checkResult (pure $ Left RefineError)) env
  where
    checkResult x _ = pure $ Right x

realization :: AST (Rt Refine) -> Env Refine -> IO (Expr Plain)
realization input env = do
  runReaderT (runLogicT (runRefine $ realize (spine input)) checkResult (error "impossible")) env
  where
    checkResult x _ = pure x

refine :: MonadRefine m => AST Plain -> m (AST (Rt m))
refine = transform $ \_start _end -> \case
  (HoleF _) -> do
    HoleF <$> newVal Unbound
  _ -> undefined

-- | Pulls values out of cells, filling holes and
-- properly annotating with types
realize :: Monad m => Expr (Rt m) -> m (Expr Plain)
realize = transverse go
  where
    go :: ExprF (Rt m) (m a) -> m (ExprF Plain a)
    go (HoleF _) = undefined
    go _ = undefined

data Rt (m :: Type -> Type)

instance ExprConfig (Rt m) where
  type LamTy (Rt m) = Val m
  type BranchTy (Rt m) = Int
  type HoleTy (Rt m) = Val m
  type RefTy (Rt m) = Val m

{-
Another issue to work through: what exactly
will the tree workon and refine?

There shouldn't be some separate tree from the
one constructed by refinement.
-}

data Stable a = Stable (StableName a) a

instance Eq (Stable a) where
  (Stable x _) == (Stable y _) = x == y

data Strict a = Strict {force :: !a}

data RtVal m
  = RtArr [RtVal m]
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
    RtLam (Stable (Val m -> m (Val m)))
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
      (RtArr xs, RtArr ys) ->
        RtArr <$> zipFail merge (Old <$> xs) (New <$> ys)
      (RtArr _, _) -> Conflict
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

data Val m where
  Val :: (Value f, Inform f, MonadRef m) => f m (Ref m) (RtVal m) -> Val m

data Env m = Env [Val m]

{-

Evaluation can happen via the propagators not the merges!
Reduction could happen via certain propagations that force eval
This allows the lattice code to be pure

-}

newVal :: RtVal m -> m (Val m)
newVal = undefined

propUnify :: (MonadRef m, Alternative m) => Cell m (Ref m) (RtVal m) -> Cell m (Ref m) (RtVal m) -> m ()
propUnify x y = do
  push [Watched x] y (value x)
  push [Watched y] x (value y)

propLambda :: MonadIO m => Strict (Val m -> m (Val m)) -> m (Val m)
propLambda (Strict f) = do
  -- Construct the body of the function
  -- with the input on the stack, now info
  -- can propagate back
  stbl <- liftIO $ makeStableName f
  newVal $ RtLam (Stable stbl f)

propApp :: (Alternative m, MonadRef m) => Val m -> Val m -> m (Val m)
propApp (Val func) (Val input) = do
  value func >>= \case
    RtLam (Stable _ f) -> do
      -- Network construction happens
      -- when the input is applied to the
      -- function
      f (Val input)
    _ -> error "Application not to a function"

propCase :: (Alternative m, MonadRef m) => Val m -> [m (Val m)] -> m (Val m)
propCase (Val subject) paths = do
  -- what would this be?
  output <- cell Unbound
  chosen <- cell (Nothing :: Maybe Int)
  push [Watched subject] chosen $ do
    value subject >>= \case
      RtCase n _ -> pure (Just n)
      _ -> error "Not a case of"
  -- This will only ever fire once
  -- because chosen is a Maybe cell
  push [Watched chosen] output $ do
    value chosen >>= \case
      Nothing -> undefined
      Just n -> do
        -- somehow make x available to the
        -- chosen path
        -- set this up so updates can
        -- still flow, the prop for result
        -- should really only happen once
        Val out <- paths !! n
        push [Watched out] output (value out)
        pure Unbound
  pure (Val output)
