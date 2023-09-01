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
import AST.Expr.Refined
import AST.Range
import AST.Tree
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Logic
import Control.Monad.Reader
import Data.Foldable
import Data.Functor.Foldable
import Data.IORef
import Data.Traversable
import Lens.Micro.Platform
import Runtime.Prop
import Runtime.Term
import System.Mem.StableName

{-
Another issue to work through: what exactly
will the tree workon and refine?

There shouldn't be some separate tree from the
one constructed by refinement.
-}

data Env m = Env {stack :: [Term m], ref :: Ref m IORef}

class
  ( MonadReader (Env m) m,
    Alternative m,
    Monad m
  ) =>
  MonadRefine m

newtype R a = Refine {runRefine :: LogicT (ReaderT (Env R) IO) a}
  deriving (Functor, Applicative, Monad, MonadReader (Env R), Alternative, MonadIO)

instance MonadRefine R

data RefineError = RefineError

refinement :: AST Plain -> Env R -> IO (Either RefineError (AST (Refined R)))
refinement input env = do
  runReaderT (runLogicT (runRefine $ refine input) checkResult (pure $ Left RefineError)) env
  where
    checkResult x _ = pure $ Right x

realization :: AST (Refined R) -> Env R -> IO (Expr Plain)
realization input env = do
  runReaderT (runLogicT (runRefine $ realize (spine input)) checkResult (error "impossible")) env
  where
    checkResult x _ = pure x

refine :: MonadRefine m => AST Plain -> m (AST (Refined m))
refine (Tree t (HoleF _)) = do
  ty <- term Unbound
  Tree (Refined (t ^. range) ty) . HoleF <$> term Unbound
refine (Tree t (ProdF xs)) = do
  elemsTy <- term Unbound
  elems <- traverse refine xs
  propProduct (view typeOf <$> elems) elemsTy
  ty <- term Unbound
  propProdTy elemsTy ty
  pure $ Tree (Refined (t ^. range) ty) $ ProdF elems
refine (Tree _ _) = undefined

{-

Evaluation can happen via the propagators not the merges!
Reduction could happen via certain propagations that force eval
This allows the lattice code to be pure

-}

value :: Monad m => Term m -> m (RtVal m)
value (Term (Cell cInfo cell)) = readValue cInfo cell

watch :: Monad m => Term m -> Watched m
watch (Term (Cell cInfo cell)) = Watched cInfo cell

term :: RtVal m -> m (Term m)
term = undefined

push :: MonadRefine m => [Watched m] -> Term m -> m (RtVal m) -> m ()
push xs (Term (Cell info target)) action = do
  refImpl <- ref <$> ask
  pushBase xs target action (FailAction undefined) info refImpl

propProdTy :: (MonadRefine m) => Term m -> Term m -> m ()
propProdTy elemsTy@(Term (Cell elemsTyInfo _)) ty@(Term (Cell tyInfo _)) = do
  push [watch elemsTy] ty $ do
    tys <- value elemsTy
    pure $ RtProdTy tys
  push [watch ty] elemsTy $ do
    value ty >>= \case
      RtProdTy tys -> pure tys
      _ -> undefined

propProduct :: (MonadRefine m) => [Term m] -> Term m -> m ()
propProduct elems prod = do
  for_ (zip [0 ..] elems) $ \(i, x) -> do
    push [watch x] prod $ do
      val <- value x
      value prod >>= \case
        RtList current -> do
          let (hd, tl) = splitAt i current
          pure . RtList $ hd <> (val : drop 1 tl)
        _ -> undefined
    push [watch prod] x $ do
      value prod >>= \case
        RtList xs -> pure $ xs !! i
        _ -> undefined

propUnify :: (MonadRefine m) => Term m -> Term m -> m ()
propUnify (x) (y) = do
  push [watch x] y (value x)
  push [watch y] x (value y)

propLambda :: MonadIO m => Strict (Term m -> m (Term m)) -> m (Term m)
propLambda (Strict f) = do
  -- Construct the body of the function
  -- with the input on the stack, now info
  -- can propagate back
  stbl <- liftIO $ makeStableName f
  term $ RtLam (Stable stbl f)

propApp :: (Monad m) => Term m -> Term m -> m (Term m)
propApp func input = do
  value func >>= \case
    RtLam (Stable _ f) -> do
      -- Network construction happens
      -- when the input is applied to the
      -- function
      f input
    _ -> error "Application not to a function"

-- propCase :: (Monad m) => Term m -> [m (Term m)] -> m (Term m)
-- propCase subject paths = do
--   -- what would this be?
--   output <- term Unbound
--   chosen <- refCell (Nothing :: Maybe Int) =<< ref <$> ask
--   push [watch subject] chosen $ do
--     value subject >>= \case
--       RtCase n _ -> pure (Just n)
--       _ -> error "Not a case of"
--   -- This will only ever fire once
--   -- because chosen is a Maybe cell
--   push [watch chosen] output $ do
--     value chosen >>= \case
--       Nothing -> undefined
--       Just n -> do
--         -- somehow make x available to the
--         -- chosen path
--         -- set this up so updates can
--         -- still flow, the prop for result
--         -- should really only happen once
--         out <- paths !! n
--         push [watch out] output (value out)
--         pure Unbound
--   pure output

-- | Pulls values out of cells, filling holes and
-- properly annotating with types
realize :: Monad m => Expr (Refined m) -> m (Expr Plain)
realize = transverse go
  where
    go :: ExprF (Refined m) (m a) -> m (ExprF Plain a)
    go (HoleF _) = undefined
    go _ = undefined
