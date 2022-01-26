{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module TypeCheck where

import AST.Expr
import AST.LocTree
import qualified AST.LocTree as AST
import Control.Applicative
import Control.Monad.Logic
import Control.Monad.State
import Data.Foldable (for_)
import qualified Data.HashSet as HS
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
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
  xValRef <- newRef @_ @IORef @TyTerm undefined
  nil <- cell ()
  origBranch <- currentBranch <$> get
  prop [Watched cellX] nil $ do
    x `copyInto` xValRef
    pure ()
  prop [Watched cellY] nil $ do
    subProblem "Ambiguity Check!" $ do
      yVal <- liftIO (gatherTy y)
      xTerm <- readRef xValRef
      xVal <- liftIO (gatherTy xTerm)
      debug $ show xVal <> " ~ " <> show yVal
      ifte
        (unify xTerm y)
        (const $ debug "All good!" *> pure ())
        ( do
            b <- currentBranch <$> get
            addErrorToBranch origBranch $ AmbiguousTypes b
            pure ()
        )
  branch [pathX x, pathY y]
  where
    copyInto term ref = do
      result <- liftIO $ gatherTy term
      -- Copy the term and store it so it isn't
      -- erased during back tracking
      writeRef ref =<< disperseTy result

-- | A wrapper around the LogicT <|> which
-- handles branching and sub problem
-- book keeping.
--
-- This should *ALWAYS* be used instead of <|>
branch :: [TyCheckM a] -> TyCheckM a
branch [] = empty
branch (x : xs) = do
  depth <- problemDepth <$> get
  Branch b <- currentBranch <$> get
  x <|> go b depth xs
  where
    go _ _ [] = empty
    go b depth (y : ys) = do
      -- Only make a new branch after following the first path
      -- as deep as it will go
      modify $ \st ->
        st
          { currentBranch = Branch (unBranch (currentBranch st) + 1),
            problemDepth = depth
          }
      Branch b' <- currentBranch <$> get
      debugNoFormat $ show b <> " -> " <> show b' <> ": " <> replicate 30 '-'
      y <|> go b depth ys

-- | Adds an error to the error list
failWith :: TCError -> TyCheckM ()
failWith e = do
  b <- currentBranch <$> get
  addErrorToBranch b e
  failBranch b

addErrorToBranch :: Branch -> TCError -> TyCheckM ()
addErrorToBranch b e = do
  debug $ show e
  modify $ \st -> st {errors = (e, b) : errors st}

failThisBranch :: TyCheckM ()
failThisBranch = do
  modify $ \st -> st {failedBranches = HS.insert (currentBranch st) (failedBranches st)}

failBranch :: Branch -> TyCheckM ()
failBranch b = do
  modify $ \st -> st {failedBranches = HS.insert b (failedBranches st)}

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
  when debugTypeChecking $ do
    putStrLn $ "Number of Solutions: " <> show (length solutions)
    putStrLn $ "Errors: " <> show (errors st)
  let remainingErrors = validErrors (snd <$> solutions) st
  when debugTypeChecking $ do
    putStrLn $ "Valid Errors: " <> show remainingErrors
  case remainingErrors of
    [] -> case solutions of
      [] -> error "No valid solutions!"
      -- It's okay if there are multiple solutions,
      -- if there wasn't an ambiguous types error
      -- reported, then they must be the same
      ((valid, _) : _) -> pure $ Right valid
    errs -> pure . Left $ fst <$> errs
  where
    go = do
      setup
      debug "Starting Type Checking!"
      solution <- convertTree tree
      st <- get
      if (currentBranch st `HS.member` failedBranches st)
        then debug "No Solution!" *> empty
        else do
          debug . ("Solution: " <>) . show =<< liftIO (treeGatherRootTy solution)
          x <- liftIO $ treeGatherAllTys solution
          pure (x, currentBranch st)

validErrors :: [Branch] -> TyCheckSt -> [(TCError, Branch)]
validErrors xs st =
  flip filter (errors st) $ \case
    (AmbiguousTypes x, y) ->
      not $ any (`HS.member` failedBranches st) [x, y]
    (_, b) -> b `elem` xs || (xs == [] && b == (Branch 0))

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
        `ifFailThen` failWith TypeMismatch
      outputTree <- inferOutput
      debugShowTreeTy outputTree
      (unify (treeRootTy outputTree) =<< tyTerm (Filled RtTyF))
        `ifFailThen` failWith TypeMismatch
      thisTy <- tyTerm $ Filled RtTyF
      pure $ TyExprF thisTy $ RtArrF inputTree outputTree

    -- PRODUCT
    go _ _ (ProdF []) = subProblem "ProdF []" $ do
      exclusive asType asValue
      where
        asType term = do
          debug "trying unit term as type"
          unify term =<< tyTerm (Filled RtTyF)
          pure $ TyExprF term unitF
        asValue term = do
          debug "trying unit as value"
          unify term =<< (tyTerm $ Filled unitF)
          pure $ TyExprF term $ unitF
    go _ _ (ProdF checkElems) = subProblem "ProdF [..]" $ do
      elemTrees <- sequence checkElems
      let tys = tyF . locContent <$> elemTrees
      term <- tyTerm Empty
      debug "trying product term as type"
      ifte
        ( do
            unify term =<< tyTerm (Filled RtTyF)
            for_ tys $ unify term
        )
        ( const $ do
            debug "all elements are types, so using as a type"
            pure $ TyExprF term $ RtProdF $ Vec.fromList elemTrees
        )
        ( do
            debug "using product as a value"
            unify term =<< (tyTerm . Filled . RtProdF $ Vec.fromList tys)
            pure $ TyExprF term $ RtProdF $ Vec.fromList elemTrees
        )

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
        `ifFailThen` failWith TypeMismatch
      valTree <- checkVal
      debugShowTreeTy valTree
      x <- treeValuesIntoTy annTree
      (unify (treeRootTy valTree) x)
        `ifFailThen` failWith TypeMismatch
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
