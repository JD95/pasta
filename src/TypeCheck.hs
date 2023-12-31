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

{-
import AST.Expr
import AST.LocTree
import qualified AST.LocTree as AST
import Control.Applicative
import Control.Monad.Logic
import Control.Monad.State
import Data.Foldable (for_)
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import Lexer (RowCol)
import Runtime (eval)
import Runtime.Types
import TypeCheck.Debug
import TypeCheck.Types

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
        else
          liftIO (treeGatherAllTys solution) >>= \case
            -- Need to rewrite the gather functions to also
            -- track the ambiguous terms for THIS check
            (gathered, []) -> do
              debug $ "Solution: " <> show (annF . locContent $ gathered)
              pure (gathered, currentBranch st)
            (_, _ : _) -> do
              failWith $ AmbiguousTypes (currentBranch st)
              empty

withTyCheckM :: TyCheckSt -> TyCheckM a -> IO ([a], TyCheckSt)
withTyCheckM st ma = runStateT (observeAllT $ runTyCheckM ma) st

noSetup :: TyCheckM ()
noSetup = pure ()

validErrors :: [Branch] -> TyCheckSt -> [(TCError, Branch)]
validErrors xs st =
  flip filter (errors st) $ \case
    (AmbiguousTypes x, y) ->
      not $ any (`HS.member` failedBranches st) [x, y]
    (_, b) -> b `elem` xs || (xs == [] && b == (Branch 0))

restoreBindings :: Map Text Binding -> TyCheckM ()
restoreBindings oldBindings =
  modify $ \st -> st {bindings = oldBindings}

evalTy :: TyTerm -> TyCheckM TyTerm
evalTy t = do
  liftIO (gatherTy t) >>= \case
    (t', []) -> disperseTy (eval t')
    (_, ts) -> do
      failWith =<< ambiguousTypes ts
      pure t

ambiguousTypes :: [TyTerm] -> TyCheckM TCError
ambiguousTypes _ = do
  AmbiguousTypes <$> (currentBranch <$> get)

convertTree :: LocTree RowCol ExprF -> TyCheckM TyTree
convertTree tree = AST.transform go tree
  where
    go :: RowCol -> RowCol -> ExprF (TyCheckM TyTree) -> TyCheckM (TyExprF TyTree)

    -- ARROW
    go _ _ (ArrF inputName inferInput inferOutput) = subProblem "ArrF" $ do
      -- Check Input
      debug "checking input type"
      inputTree <- inferInput
      debugShowTreeTy inputTree
      expectedInputTy <- tyTerm (Filled RtTyF)
      (unify (treeRootTy inputTree) expectedInputTy)
        `ifFailThen` (failWith =<< typeMismatch inputTree expectedInputTy)
      -- Check Output
      debug "checking output type"
      outputTree <- case inputName of
        Just t -> do
          depth <- lambdaDepth <$> get
          tTy <- evalTy =<< treeValuesIntoTy inputTree
          assuming t (DepTyBound tTy depth) $
            inferOutput
        Nothing -> do
          oldDepth <- depTyDepth <$> get
          oldStack <- depTyStack <$> get
          tTy <- treeValuesIntoTy inputTree
          modify $ \st -> st {depTyDepth = oldDepth + 1, depTyStack = oldStack Seq.|> tTy}
          result <- inferOutput
          modify (\st -> st {depTyDepth = oldDepth, depTyStack = oldStack})
          pure result
      debugShowTreeTy outputTree
      expectedOutputTy <- tyTerm (Filled RtTyF)
      (unify (treeRootTy outputTree) expectedOutputTy)
        `ifFailThen` (failWith =<< typeMismatch outputTree expectedOutputTy)
      thisTy <- tyTerm $ Filled RtTyF
      pure $ TyExprF thisTy $ RtArrF inputTree outputTree

    -- PRODUCT
    go _ _ (ProdF []) = subProblem "ProdF []" $ do
      term <- tyTerm $ Ambiguous (RtTyF :| [unitF])
      pure $ TyExprF term unitF
    go _ _ (ProdF checkElems) = subProblem "ProdF [..]" $ do
      elemTrees <- sequence checkElems
      let tys = tyF . locContent <$> elemTrees
      term <- tyTerm Empty
      ifte
        ( do
            unify term =<< tyTerm (Filled RtTyF)
            -- TODO: This check should be if *any* of the tys
            -- are Type, then the whole things must be a type
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
    go _ _ (LamF name checkBody) = subProblem "LamF" $ do
      depth <- lambdaDepth <$> get
      inputTy <- tyTerm Empty
      bodyTree <- assuming name (LambdaBound inputTy depth) $ checkBody
      thisTy <- tyTerm $ Filled $ RtArrF inputTy (treeRootTy bodyTree)
      pure $ TyExprF thisTy (RtLamF bodyTree)

    -- APPLICATION
    go _ _ (AppF checkFunc checkInputs) = subProblem "AppF" $ do
      funcTree <- checkFunc
      debugShowTreeTy funcTree
      inputTrees <- sequence checkInputs
      outTy <- tyTerm Empty
      expectedFuncTy <- funcTy (treeRootTy <$> inputTrees) outTy
      unify (treeRootTy funcTree) expectedFuncTy
        `ifFailThen` (failWith =<< typeMismatch funcTree expectedFuncTy)
      pure $ TyExprF outTy $ RtAppF funcTree (Vec.fromList inputTrees)
      where
        funcTy :: [TyTerm] -> TyTerm -> TyCheckM TyTerm
        funcTy [] outTy = pure outTy
        funcTy (t : ts) outTy = do
          output <- funcTy ts outTy
          tyTerm $ Filled $ RtArrF t output

    -- ANNOTATION
    go _ _ (AnnF checkVal checkAnn) = subProblem "AnnF" $ do
      debug "inferring type of annotation..."
      annTree <- checkAnn
      debugShowTreeTy annTree
      expectedAnnTy <- tyTerm (Filled RtTyF)
      unify (treeRootTy annTree) expectedAnnTy
        `ifFailThen` (failWith =<< typeMismatch annTree expectedAnnTy)
      valTree <- checkVal
      debugShowTreeTy valTree
      x <- evalTy =<< treeValuesIntoTy annTree
      unify (treeRootTy valTree) x
        `ifFailThen` (failWith =<< typeMismatch valTree x)
      debug "AnnF done"
      pure $ locContent valTree

    -- TYPE
    go _ _ (SymbolF "Type") = do
      t <- tyTerm (Filled RtTyF)
      pure $ TyExprF t RtTyF

    -- SYMBOL
    go x y (SymbolF name) = subProblem "SymbolF" $ do
      debug $ "looking up type for symbol " <> Text.unpack name
      result <- exprToTree x y <$> lookupBinding name
      (resultTy, _) <- liftIO $ gatherTy $ tyF result
      debug $ "type of '" <> Text.unpack name <> "' is '" <> displayRtVal resultTy <> "'"
      pure result
    go _ _ _ = undefined

lookupBinding :: Text -> TyCheckM TyExpr
lookupBinding name = do
  table <- bindings <$> get
  case Map.lookup name table of
    Just (LambdaBound t depth) -> do
      pure $ TyExpr t $ RtVarF depth
    Just (DepTyBound t depth) -> do
      pure $ TyExpr t $ RtDepTyF depth
    Just (Other result) -> pure result
    Nothing -> error "No type for symbol"

typeMismatch :: TyTree -> TyTerm -> TyCheckM TCError
typeMismatch tree term = do
  actual <- liftIO $ treeGatherRootTy tree
  (expected, _) <- liftIO $ gatherTy term
  pure $ TypeMismatch (locStart tree) (locEnd tree) actual expected

-- | If the first path fails and back tracks
-- then throw out the errors accumulated by
-- that path and try the second one.
ifFailThen :: TyCheckM a -> TyCheckM a -> TyCheckM a
ifFailThen firstOpt secondOpt = do
  oldErrors <- errors <$> get
  ifte firstOpt pure $ do
    modify $ \st -> st {errors = oldErrors}
    secondOpt

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

branchIfFails :: TyCheckM a -> TyCheckM a -> TyCheckM a
branchIfFails orig backup = do
  b <- currentBranch <$> get
  depth <- problemDepth <$> get
  orig <|> do
    fails <- failedBranches <$> get
    if b `HS.member` fails
      then do
        modify $ \st ->
          st
            { currentBranch = Branch (unBranch (currentBranch st) + 1),
              problemDepth = depth
            }
        Branch b' <- currentBranch <$> get
        debugNoFormat $ show (unBranch b) <> " -> " <> show b' <> ": " <> replicate 30 '-'
        backup
      else empty

-- | Adds an error to the error list
failWith :: TCError -> TyCheckM ()
failWith e = do
  b <- currentBranch <$> get
  addErrorToBranch b e
  failBranch b

addErrorToBranch :: Branch -> TCError -> TyCheckM ()
addErrorToBranch b e = do
  case e of
    TypeMismatch _ _ actual expected ->
      debugErr $ "Type Mismatch: '" <> show actual <> "' is not '" <> show expected <> "'"
    AmbiguousTypes _ -> debug "Ambiguous Types"
  modify $ \st -> st {errors = (e, b) : errors st}

failThisBranch :: TyCheckM ()
failThisBranch = do
  modify $ \st -> st {failedBranches = HS.insert (currentBranch st) (failedBranches st)}

failBranch :: Branch -> TyCheckM ()
failBranch b = do
  modify $ \st -> st {failedBranches = HS.insert b (failedBranches st)}

-- | Returns the bindings before the assumption so
-- they can be restored afterward
assuming :: Text -> Binding -> TyCheckM a -> TyCheckM a
assuming name t action = do
  oldBindings <- bindings <$> get
  case t of
    Other _ -> do
      modify $ \st ->
        st {bindings = Map.insert name t (bindings st)}
      result <- action
      modify $ \st ->
        st {bindings = oldBindings}
      pure result
    LambdaBound x _ -> do
      oldDepth <- lambdaDepth <$> get
      oldStack <- lamStack <$> get
      modify $ \st ->
        st
          { bindings = Map.insert name t (bindings st),
            lambdaDepth = oldDepth + 1,
            lamStack = oldStack Seq.|> x
          }
      result <- action
      modify $ \st ->
        st
          { bindings = oldBindings,
            lambdaDepth = oldDepth,
            lamStack = oldStack
          }
      pure result
    DepTyBound x _ -> do
      oldDepth <- depTyDepth <$> get
      oldStack <- depTyStack <$> get
      modify $ \st ->
        st
          { bindings = Map.insert name t (bindings st),
            depTyDepth = oldDepth + 1,
            depTyStack = oldStack Seq.|> x
          }
      result <- action
      modify $ \st ->
        st
          { bindings = oldBindings,
            depTyDepth = oldDepth,
            depTyStack = oldStack
          }
      pure result

assuming' :: Text -> Binding -> TyCheckM ()
assuming' name t = do
  modify $ \st -> st {bindings = Map.insert name t (bindings st)}
-}
