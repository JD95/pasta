{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module TypeCheck where

import AST.Expr
import AST.LocTree
import qualified AST.LocTree as AST
import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
import Data.Foldable (for_)
import qualified Data.HashSet as HS
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import Lexer (RowCol)
import Runtime.Prop
import Runtime.Ref
import Runtime.Term
import Runtime.Types
import TypeCheck.Debug
import TypeCheck.Types

-- | If the first path fails and back tracks
-- then throw out the errors accumulated by
-- that path and try the second one.
ifFailThen :: TyCheckM a -> TyCheckM a -> TyCheckM a
ifFailThen firstOpt secondOpt = do
  oldErrors <- errors <$> get
  ifte firstOpt pure $ do
    modify $ \st -> st {errors = oldErrors}
    secondOpt

-- | Throws an Ambiguity error if both terms gain info
-- If both branches throw arrows, it'll only keep errors from first branch
exclusive :: (TyTerm -> TyCheckM a) -> (TyTerm -> TyCheckM a) -> TyCheckM a
exclusive pathX pathY = do
  x <- tyTerm Empty
  y <- tyTerm Empty
  RootInfo cellX _ _ _ _ <- rootInfo (unTyTerm x)
  RootInfo cellY _ _ _ _ <- rootInfo (unTyTerm y)
  errCell <- cell Nothing
  prop [Watched cellX, Watched cellY] errCell $ do
    pure (Just AmbiguousTypes)
  b <- currentBranch <$> get
  modify $ \st -> st {errors = (errCell, b) : errors st}
  branch [pathX x, pathY y]

-- | A wrapper around the LogicT <|> which
-- handles branching and sub problem
-- book keeping.
--
-- This should *ALWAYS* be used instead of <|>
branch :: [TyCheckM a] -> TyCheckM a
branch [] = empty
branch (x : xs) = do
  depth <- problemDepth <$> get
  x <|> go depth xs
  where
    go _ [] = empty
    go depth (y : ys) = do
      -- Only make a new branch after following the first path
      -- as deep as it will go
      modify $ \st ->
        st
          { currentBranch = Branch (unBranch (currentBranch st) + 1),
            problemDepth = depth
          }
      y <|> go depth ys

-- | Adds an error to the error list
addError :: TCError -> TyCheckM a
addError e = do
  debug "Error! Backtracking..."
  errCell <- cell $ Just e
  b <- currentBranch <$> get
  debug $ show b <> " has failed!"
  modify $ \st ->
    st
      { failedBranches = HS.insert b (failedBranches st),
        errors = (errCell, b) : errors st
      }
  empty

-- | Returns the bindings before the assumption so
-- they can be restored afterward
assuming :: Text -> Binding -> TyCheckM (Map Text Binding)
assuming name t = do
  oldBindings <- bindings <$> get
  modify $ \st -> st {bindings = Map.insert name t (bindings st)}
  pure oldBindings

withTyCheckM :: TyCheckSt -> TyCheckM a -> IO ([a], TyCheckSt)
withTyCheckM st ma = runStateT (observeAllT $ runTyCheckM ma) st

typeCheck :: LocTree RowCol ExprF -> TyCheckSt -> TyCheckM () -> IO (Either [TCError] (LocTree RowCol GatheredF))
typeCheck tree initSt setup = do
  (solutions, st) <- runStateT (observeAllT $ runTyCheckM go) initSt
  filledErrors (snd <$> solutions) st >>= \case
    [] -> case solutions of
      [] -> error "No valid solutions!"
      [(valid, _)] -> pure $ Right valid
      (_ : _ : _) -> pure $ Left [AmbiguousTypes]
    errs -> Left . catMaybes <$> traverse (readRef @IO @IORef . value . fst) errs
  where
    go = do
      setup
      debug "Starting Type Checking!"
      solution <- convertTree tree
      debug . ("Solution: " <>) . show =<< liftIO (treeGatherRootTy solution)
      b <- currentBranch <$> get
      x <- liftIO $ treeGatherAllTys solution
      pure (x, b)

    filledErrors xs st =
      flip filterM (errors st) $ \(e, b) -> do
        if b `elem` xs || (xs == [] && b == (Branch 0))
          then isJust <$> readRef (value e)
          else pure False

noSetup :: TyCheckM ()
noSetup = pure ()

restoreBindings :: Map Text Binding -> TyCheckM ()
restoreBindings oldBindings =
  modify $ \st -> st {bindings = oldBindings}

convertTree :: LocTree RowCol ExprF -> TyCheckM TyTree
convertTree tree = AST.transform go tree
  where
    go :: RowCol -> RowCol -> ExprF (TyCheckM TyTree) -> TyCheckM (TyExprF TyTree)

    -- ARROW
    go _ _ (ArrF inferInput inferOutput) = subProblem "ArrF" $ do
      inputTree <- inferInput
      debugShowTreeTy inputTree
      (unify (treeRootTy inputTree) =<< tyTerm (Filled RtTyF))
        `ifFailThen` addError TypeMismatch
      outputTree <- inferOutput
      debugShowTreeTy outputTree
      (unify (treeRootTy outputTree) =<< tyTerm (Filled RtTyF))
        `ifFailThen` addError TypeMismatch
      thisTy <- tyTerm $ Filled RtTyF
      pure $ TyExprF thisTy $ RtArrF inputTree outputTree

    -- PRODUCT
    go _ _ (ProdF checkElems) = subProblem "ProdF" $ do
      elemTrees <- sequence checkElems
      let tys = tyF . locContent <$> elemTrees
      exclusive
        (asType elemTrees tys)
        (asValue elemTrees tys)
      where
        asType elemTrees tys term = do
          debug "trying product term as type"
          unify term =<< tyTerm (Filled RtTyF)
          for_ tys $ unify term
          debug "all elements are type"
          pure $ TyExprF term $ RtProdF $ Vec.fromList elemTrees
        asValue elemTrees tys term = do
          debug "trying product as value"
          unify term =<< (tyTerm . Filled . RtProdF $ Vec.fromList tys)
          pure $ TyExprF term $ RtProdF $ Vec.fromList elemTrees

    -- LAMBDA
    go _ _ (LamF checkName checkBody) = subProblem "LamF" $ do
      depth <- lambdaDepth <$> get
      inputTy <- tyTerm Empty
      oldBindings <- assuming checkName $ LambdaBound inputTy depth
      bodyTree <- checkBody
      restoreBindings oldBindings
      thisTy <- tyTerm $ Filled $ RtArrF inputTy (treeRootTy bodyTree)
      pure $ TyExprF thisTy (RtLamF bodyTree)

    -- ANNOTATION
    go _ _ (AnnF checkVal checkAnn) = subProblem "AnnF" $ do
      debug "inferring type of annotation..."
      annTree <- checkAnn
      debugShowTreeTy annTree
      (unify (treeRootTy annTree) =<< tyTerm (Filled RtTyF))
        `ifFailThen` addError TypeMismatch
      valTree <- checkVal
      debugShowTreeTy valTree
      x <- treeValuesIntoTy annTree
      (unify (treeRootTy valTree) x)
        `ifFailThen` addError TypeMismatch
      debug "AnnF done"
      pure $ locContent valTree

    -- SYMBOL
    go x y (SymbolF name) = subProblem "SymbolF" $ do
      debug $ "looking up type for symbol " <> Text.unpack name
      exprToTree x y <$> lookupBinding name
    go _ _ _ = undefined

lookupBinding :: Text -> TyCheckM TyExpr
lookupBinding name = do
  table <- bindings <$> get
  case Map.lookup name table of
    Just (LambdaBound t depth) -> do
      thisDepth <- lambdaDepth <$> get
      pure $ TyExpr t $ RtVarF $ thisDepth - depth
    Just (Other result) -> pure result
    Nothing -> error "No type for symbol"
