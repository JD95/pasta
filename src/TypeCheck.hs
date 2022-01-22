{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module TypeCheck where

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

data TyCheckSt = TyCheckSt
  { bindings :: Map Text Binding,
    lambdaDepth :: Word32,
    uidList :: [Word32],
    errors :: [TCError]
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
data TyCell = TyCell {unTyCell :: Term Word32 IORef (Cell TyCheckM IORef (Hole (RtValF TyCell)))}
  deriving (Eq)

unify x y = (unTyCell x) `is` (unTyCell y)

newUid = state $ \s -> (Prelude.head $ uidList s, s { uidList = Prelude.tail (uidList s) })

tyCell value = do
  uid <- newUid
  TyCell <$> (newTerm uid =<< cell value)

data TCheckEnv = TCheckEnv

data TyExpr = TyExpr {ty :: TyCell, expr :: (RtValF TyExpr)}

makeBaseFunctor ''TyExpr

type TyTree = LocTree RowCol TyExprF

defaultTyCheckSt :: TyCheckSt
defaultTyCheckSt =
  TyCheckSt
    { bindings = Map.empty,
      lambdaDepth = 0,
      uidList = [0..], -- TODO: Do better
      errors = []
    }

-- Must be below the template haskell
instance Lattice TyCheckM (Hole (RtValF TyCell)) where
  merge (Old (Filled (RtProdF xs))) (New (Filled (RtProdF ys))) = do
    if Vec.length xs /= Vec.length ys
      then pure Conflict
      else do
        for_ (Vec.zip xs ys) (uncurry unify)
        pure None
  merge (Old (Filled RtTyF)) (New (Filled RtTyF)) = pure None
  merge (Old (Filled RtTyF)) (New (Filled _)) = pure Conflict
  merge (Old (Filled _)) (New (Filled RtTyF)) = pure Conflict
  merge (Old (Filled x)) (New (Filled y)) = do
    xVal <- liftIO $ embed <$> traverse saturateTy x
    yVal <- liftIO $ embed <$> traverse saturateTy y
    error $ "Haven't fully implemented merge for TyCells yet:\n " <> show xVal <> "\n" <> show yVal
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

-- | Succeeds only if one of the paths succeed
exclusiveAlt :: TyCheckM a -> TyCheckM a -> TyCheckM a
exclusiveAlt x y = do
  ifte
    -- If x succeeds, then y must fail
    x
    (\a ->
       -- x succeeded
       ifte
       y
       -- y also succeeded, so now there's an ambiguity
       (const $ addError AmbiguousTypes)
       -- y failed so just use the result of x
       (pure a))
    -- x failed so just use the result of y
    y


-- | Adds an error to the error list
addError :: TCError -> TyCheckM a
addError e = do
  modify $ \st -> st { errors = e : errors st }
  Control.Applicative.empty

-- | Returns the bindings before the assumption so
-- they can be restored afterward
assuming :: Text -> Binding -> TyCheckM (Map Text Binding)
assuming name ty = do
  oldBindings <- bindings <$> get
  modify $ \st -> st {bindings = Map.insert name ty (bindings st)}
  pure oldBindings

typeCheck :: LocTree RowCol ExprF -> TyCheckSt -> TyCheckM () -> IO (Either [TCError] TyTree)
typeCheck tree initSt setup = do
  (solutions, st) <- runStateT (observeAllT $ runTyCheckM $ setup *> convertTree tree) initSt
  case errors st of
    [] -> case solutions of
      [] -> error ""
      [valid] -> pure $ Right valid
      (_:_:_) -> pure $ Left [AmbiguousTypes]
    errs -> pure $ Left errs

noSetup :: TyCheckM ()
noSetup = pure ()

restoreBindings :: Map Text Binding -> TyCheckM ()
restoreBindings oldBindings =
  modify $ \st -> st { bindings = oldBindings }

convertTree :: LocTree RowCol ExprF -> TyCheckM TyTree
convertTree tree = AST.transform go tree
  where
    go :: RowCol -> RowCol -> ExprF (TyCheckM TyTree) -> TyCheckM (TyExprF TyTree)
    -- PRODUCT
    go _ _ (ProdF checkElems) = do
      elemTrees <- sequence checkElems
      let tys = tyF . locContent <$> elemTrees
      asValue elemTrees tys `exclusiveAlt` asType elemTrees tys
      where
        asType elemTrees tys = do
          tyTerm <- tyCell $ Filled RtTyF
          for_ tys $ unify tyTerm
          pure $ TyExprF tyTerm $ RtProdF $ Vec.fromList elemTrees
        asValue elemTrees tys = do
          thisTy <- tyCell . Filled . RtProdF $ Vec.fromList tys
          pure $ TyExprF thisTy $ RtProdF $ Vec.fromList elemTrees
    -- LAMBDA
    go _ _ (LamF checkName checkBody) = do
      depth <- lambdaDepth <$> get
      inputTy <- tyCell Empty
      oldBindings <- assuming checkName $ LambdaBound inputTy depth
      bodyTree <- checkBody
      restoreBindings oldBindings
      pure $ TyExprF (tyF $ locContent bodyTree) (RtLamF bodyTree)
    -- ANNOTATION
    go _ _ (AnnF checkTerm checkAnn) = do
      annTree <- checkAnn
      tyTerm <- tyCell $ Filled RtTyF
      unify (tyOf annTree) tyTerm `ifFailThen` addError TypeMismatch
      termTree <- checkTerm
      annTerm <- tyCell $ Filled $ unwrapVal annTree
      unify (tyOf termTree) annTerm `ifFailThen` addError TypeMismatch
      pure $ locContent termTree
    -- SYMBOL
    go x y (SymbolF name) = do
      wrapExpr x y <$> lookupValFor name

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

wrapExpr :: RowCol -> RowCol -> TyExpr -> TyExprF TyTree
wrapExpr start end (TyExpr thisTy val) = TyExprF thisTy $ (LocTree start end . wrapExpr start end <$> val)

unwrapVal :: TyTree -> RtValF TyCell
unwrapVal (LocTree _ _ (TyExprF ty valTree)) = tyF . locContent <$> valTree

extractTy :: TyTree -> IO RtVal
extractTy tree = saturateTy $ tyF $ locContent tree

saturateTy :: TyCell -> IO RtVal
saturateTy (TyCell tyCell) = do
  (thisCell, _, _, _) <- rootInfo tyCell
  readRef (value thisCell) >>= \case
    Filled (RtProdF xs) -> RtProd <$> traverse saturateTy xs
    Filled _ -> undefined
    Empty -> undefined

extractValue :: TyTree -> IO RtVal
extractValue tree =
  go . exprF $ locContent tree
  where
    go :: RtValF TyTree -> IO RtVal
    go (RtProdF xs) = RtProd <$> traverse (go . exprF . locContent) xs
    go _ = undefined
