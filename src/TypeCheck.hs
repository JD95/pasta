{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module TypeCheck where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Control.Applicative
import Data.Foldable (for_)
import AST.Expr
import AST.LocTree
import qualified AST.LocTree as AST
import Control.Exception
import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
import Data.IORef
import Data.Functor.Foldable (embed)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import qualified Data.Vector as Vec
import Data.Word
import Lexer (RowCol)
import Runtime.Prop
import Runtime.Ref
import Runtime.Term
import Runtime.Types
import Data.HashSet (HashSet)
import Data.Hashable
import qualified Data.HashSet as HS

type ErrorCell = Cell TyCheckM IORef (Maybe TCError)

newtype Branch = Branch { unBranch :: Word32 }
  deriving (Show, Eq, Hashable)

data TyCheckSt = TyCheckSt
  { bindings :: Map Text Binding,
    lambdaDepth :: Word32,
    uidList :: [Word32],
    errors :: [(ErrorCell, Branch)],
    currentBranch :: Branch,
    failedBranches :: HashSet Branch,
    problemDepth :: Word32
  }

data TCError
  = TypeMismatch
  | AmbiguousTypes
  | NoAnswer
  deriving (Show, Eq)

instance Exception TCError where
  toException e = SomeException e
  fromException (SomeException e) = cast e
  displayException TypeMismatch = "Type Mistmatch!"
  displayException AmbiguousTypes = "Ambiguous Types!"

data Binding
  = LambdaBound
      TyCell
    -- | The depth of the lambda from the root, used to
    -- later calculate the De Bruijn indicies
    --
    -- @
    -- \x -> \y -> \z -> ...
    -- @
    --
    -- The values inserted into the bindings map would be
    -- respectively: 0, 1, 2
    --
    -- When the values are looked up, the difference is
    -- taken between the current depth and the depth
    -- of the lambda that bound the value
    --
    -- @
    -- \x -> \f -> f x
    -- @
    --
    -- becomes
    --
    -- @
    -- RtLam (RtLam (RtApp (RtVar 0) (RtVar 1)))
    -- @
      Word32
  | Other TyExpr
newtype TyCheckM a = TyCheckM {runTyCheckM :: LogicT (StateT TyCheckSt IO) a}
  deriving (Functor, Applicative, Monad, Alternative, MonadState TyCheckSt, MonadLogic, MonadIO)

instance Ref TyCheckM IORef where
  newRef = liftIO . newIORef
  readRef = liftIO . readIORef
  writeRef r = liftIO . writeIORef r

data Hole a
  = Filled a
  | Empty
  deriving (Eq)

-- |
-- The Term layer allows for efficient unification
-- The Cell layer gives us propagation and merging
newtype TyCell = TyCell {unTyCell :: Term Word32 IORef (Cell TyCheckM IORef (Hole (RtValF TyCell)))}
  deriving (Eq)

unify x y = (unTyCell x) `is` (unTyCell y)

newUid = state $ \s -> (Prelude.head $ uidList s, s { uidList = Prelude.tail (uidList s) })

tyCell :: Hole (RtValF TyCell) -> TyCheckM TyCell
tyCell value = do
  uid <- newUid
  TyCell <$> (newTerm uid =<< cell value)

data TCheckEnv = TCheckEnv

data TyExpr = TyExpr {ty :: TyCell, expr :: (RtValF TyExpr)}

makeBaseFunctor ''TyExpr

data Annotated = Annotated {ann :: RtVal, body :: (RtValF Annotated)}

makeBaseFunctor ''Annotated

type TyTree = LocTree RowCol TyExprF

defaultTyCheckSt :: TyCheckSt
defaultTyCheckSt =
  TyCheckSt
    { bindings = Map.empty,
      lambdaDepth = 0,
      uidList = [0..], -- TODO: Do better
      errors = [],
      currentBranch = Branch 0,
      failedBranches = HS.empty,
      problemDepth = 0
    }

-- Must be below the template haskell
instance Lattice TyCheckM (Hole (RtValF TyCell)) where
  -- TYPE
  merge (Old (Filled RtTyF)) (New (Filled RtTyF)) = pure None
  merge (Old (Filled RtTyF)) (New (Filled _)) = pure Conflict
  merge (Old (Filled _)) (New (Filled RtTyF)) = pure Conflict
  -- PRODUCTS
  merge (Old (Filled (RtProdF xs))) (New (Filled (RtProdF ys))) = do
    if Vec.length xs /= Vec.length ys
      then pure Conflict
      else do
        for_ (Vec.zip xs ys) (uncurry unify)
        pure None
  -- ARROWS
  merge (Old (Filled (RtArrF a b))) (New (Filled (RtArrF c d))) = do
    unify a c
    unify b d
    pure None
  merge (Old (Filled x)) (New (Filled y)) = do
    xVal <- liftIO $ embed <$> traverse gatherTy x
    yVal <- liftIO $ embed <$> traverse gatherTy y
    error $ "Haven't fully implemented merge for TyCells yet:\n" <> show xVal <> "\n" <> show yVal
  merge (Old Empty) (New (Filled y)) = pure $ Gain (Filled y)
  merge _ (New Empty) =  pure None

  bottom = pure Empty

  isTop (Filled _) = pure True
  isTop Empty = pure False

ifFailThen :: TyCheckM a -> TyCheckM a -> TyCheckM a
ifFailThen firstOpt secondOpt = do
  oldErrors <- errors <$> get
  ifte firstOpt pure $ do
    modify $ \st -> st { errors = oldErrors }
    secondOpt

-- | Throws an Ambiguity error if both terms gain info
-- If both branches throw arrows, it'll only keep errors from first branch
exclusive :: (TyCell -> TyCheckM a) -> (TyCell -> TyCheckM a) -> TyCheckM a
exclusive pathX pathY = do
  x <- tyCell Empty
  y <- tyCell Empty
  RootInfo cellX _ _ _ _ <- rootInfo (unTyCell x)
  RootInfo cellY _ _ _ _ <- rootInfo (unTyCell y)
  errCell <- cell Nothing
  prop [Watched cellX, Watched cellY] errCell $ do
    pure (Just AmbiguousTypes)
  b <- currentBranch <$> get
  modify $ \st -> st { errors = (errCell, b) : errors st }
  branch [pathX x, pathY y]

branch :: [TyCheckM a] -> TyCheckM a
branch [] = empty
branch (x : xs) = do
  depth <- problemDepth <$> get
  x <|> go depth xs

  where

  go _ [] = empty
  go depth (y:ys) = do
    -- Only make a new branch after following the first path
    -- as deep as it will go
    modify $ \st -> st
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
  modify $ \st -> st { failedBranches = HS.insert b (failedBranches st), errors = (errCell, b) : errors st }
  empty

-- | Returns the bindings before the assumption so
-- they can be restored afterward
assuming :: Text -> Binding -> TyCheckM (Map Text Binding)
assuming name ty = do
  oldBindings <- bindings <$> get
  modify $ \st -> st {bindings = Map.insert name ty (bindings st)}
  pure oldBindings

withTyCheckM :: TyCheckSt -> TyCheckM a -> IO ([a], TyCheckSt)
withTyCheckM st ma = runStateT (observeAllT $ runTyCheckM ma) st

typeCheck :: LocTree RowCol ExprF -> TyCheckSt -> TyCheckM () -> IO (Either [TCError] (LocTree RowCol AnnotatedF))
typeCheck tree initSt setup = do
  (solutions, st) <- runStateT (observeAllT $ runTyCheckM go) initSt
  filledErrors (snd <$> solutions) st >>= \case
    [] -> case solutions of
      [] -> error "No valid solutions!"
      [(valid, _)] -> pure $ Right valid
      (_:_:_) -> pure $ Left [AmbiguousTypes]
    errs -> Left . catMaybes <$> traverse (readRef @IO @IORef . value . fst) errs
  where
    go = do
      debug "Starting Type Checking!"
      setup
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
  modify $ \st -> st { bindings = oldBindings }

convertTree :: LocTree RowCol ExprF -> TyCheckM TyTree
convertTree tree = AST.transform go tree
  where
    go :: RowCol -> RowCol -> ExprF (TyCheckM TyTree) -> TyCheckM (TyExprF TyTree)

    -- ARROW
    go _ _ (ArrF inferInput inferOutput) = subProblem "ArrF" $ do
      inputTree <- inferInput
      debugShowTreeTy inputTree
      (unify (tyOf inputTree) =<< tyCell (Filled RtTyF))
        `ifFailThen` addError TypeMismatch
      outputTree <- inferOutput
      debugShowTreeTy outputTree
      (unify (tyOf outputTree) =<< tyCell (Filled RtTyF))
        `ifFailThen` addError TypeMismatch
      thisTy <- tyCell $ Filled RtTyF
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
          unify term =<< tyCell (Filled RtTyF)
          for_ tys $ unify term
          debug "all elements are type"
          pure $ TyExprF term $ RtProdF $ Vec.fromList elemTrees
        asValue elemTrees tys term = do
          debug "trying product as value"
          thisTy <- tyCell . Filled . RtProdF $ Vec.fromList tys
          pure $ TyExprF thisTy $ RtProdF $ Vec.fromList elemTrees

    -- LAMBDA
    go _ _ (LamF checkName checkBody) = subProblem "LamF" $ do
      depth <- lambdaDepth <$> get
      inputTy <- tyCell Empty
      oldBindings <- assuming checkName $ LambdaBound inputTy depth
      bodyTree <- checkBody
      restoreBindings oldBindings
      thisTy <- tyCell $ Filled $ RtArrF inputTy (tyOf bodyTree)
      pure $ TyExprF thisTy (RtLamF bodyTree)

    -- ANNOTATION
    go _ _ (AnnF checkTerm checkAnn) = subProblem "AnnF" $ do
      debug "inferring type of annotation..."
      annTree <- checkAnn
      debugShowTreeTy annTree
      (unify (tyOf annTree) =<< tyCell (Filled RtTyF))
        `ifFailThen` addError TypeMismatch
      termTree <- checkTerm
      debugShowTreeTy termTree
      x <- treeValuesIntoTy annTree
      debug "going to unify annotation "
      debug . show =<< liftIO (gatherTy x)
      debug "with term..."
      debug . show =<< liftIO (treeGatherRootTy termTree)
      (unify (tyOf termTree) x)
        `ifFailThen` addError TypeMismatch
      debug "term is now..."
      debug . show =<< liftIO (treeGatherRootTy termTree)
      debug "AnnF done"
      pure $ locContent termTree

    -- SYMBOL
    go x y (SymbolF name) = subProblem "SymbolF" $ do
      debug $ "looking up type for symbol " <> Text.unpack name
      exprToTree x y <$> lookupValFor name

tyOf :: TyTree -> TyCell
tyOf = tyF . locContent

lookupValFor :: Text -> TyCheckM TyExpr
lookupValFor name = do
  table <- bindings <$> get
  case Map.lookup name table of
    Just (LambdaBound ty depth) -> do
      thisDepth <- lambdaDepth <$> get
      pure $ TyExpr ty $ RtVarF $ thisDepth - depth
    Just (Other result) -> pure result
    Nothing -> error "No type for symbol"

exprToTree :: RowCol -> RowCol -> TyExpr -> TyExprF TyTree
exprToTree start end (TyExpr thisTy val) = TyExprF thisTy $ (LocTree start end . exprToTree start end <$> val)

treeValuesIntoTy :: TyTree -> TyCheckM TyCell
treeValuesIntoTy (LocTree _ _ (TyExprF ty valTree)) = do
  tyCell . Filled =<< traverse treeValuesIntoTy valTree

treeGatherAllTys :: LocTree RowCol TyExprF -> IO (LocTree RowCol AnnotatedF)
treeGatherAllTys (LocTree x y (TyExprF t val)) = do
  val' <- AnnotatedF <$> gatherTy t <*> traverse treeGatherAllTys val
  pure $ LocTree x y val'

treeGatherRootTy :: TyTree -> IO RtVal
treeGatherRootTy tree = gatherTy $ tyF $ locContent tree

gatherTy :: TyCell -> IO RtVal
gatherTy c = evalStateT (go c) (Map.empty, 0) where

  go :: TyCell -> StateT (Map Word32 Word32, Word32) IO RtVal
  go (TyCell tyCell) = do
    RootInfo thisCell _ uid _ _ <- liftIO $ rootInfo tyCell
    liftIO (readRef $ value thisCell) >>= \case
      Filled (RtProdF xs) -> RtProd <$> traverse go xs
      Filled (RtArrF input output) -> RtArr <$> go input <*> go output
      Filled RtTyF -> pure RtTy
      Empty -> Map.lookup uid . fst <$> get >>= \case
        Just i -> pure $ RtVar i
        Nothing -> do
          (tbl, n) <- get
          put $ (Map.insert uid n tbl, n + 1)
          pure $ RtVar n

treeStripTypes :: TyTree -> RtVal
treeStripTypes (LocTree _ _ (TyExprF _ e)) =
  embed $ treeStripTypes <$> e

debugTypeChecking = False

debug msg =
  when debugTypeChecking $ do
    depth <- problemDepth <$> get
    liftIO $ putStrLn $ replicate (fromIntegral $ depth * 2) ' ' <> msg

debugShowTreeTy tree = do
  let val = treeStripTypes tree
  t <- liftIO $ treeGatherRootTy tree
  debug $ "type of " <> show val <> " is " <> show t

subProblem msg check = do
  debug msg
  modify $ \st -> st { problemDepth = problemDepth st + 1 }
  result <- check
  modify $ \st -> st { problemDepth = problemDepth st - 1 }
  pure result
