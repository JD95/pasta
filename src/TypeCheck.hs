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
import AST.Expr
import AST.LocTree
import qualified AST.LocTree as AST
import Control.Exception
import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
import Data.IORef
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text
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
    uidList :: [Word32]
  }

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
newtype TyCheckM a = TyCheckM {runTyCheckM :: StateT TyCheckSt (LogicT IO) a}
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

instance Lattice TyCheckM (Hole (RtValF TyCell)) where
  bottom = pure Empty
  isTop (Filled _) = pure True
  isTop Empty = pure False

  merge (Old (Filled x)) (New (Filled y)) = error "Haven't implemented merge for TyCells yet"
  merge (Old Empty) (New (Filled y)) = pure $ Gain (Filled y)
  merge _ (New Empty) =  pure None

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
      uidList = [0..] -- TODO: Do better
    }

data TCError
  = TypeMismatch
  | AmbiguousTypes
  deriving (Show)

instance Exception TCError where
  toException e = SomeException e
  fromException (SomeException e) = cast e
  displayException TypeMismatch = "Type Mistmatch!"
  displayException AmbiguousTypes = "Ambiguous Types!"

-- | Returns the bindings before the assumption so
-- they can be restored afterward
assuming :: Text -> Binding -> TyCheckM (Map Text Binding)
assuming name ty = do
  oldBindings <- bindings <$> get
  modify $ \st -> st {bindings = Map.insert name ty (bindings st)}
  pure oldBindings

typeCheck :: LocTree RowCol ExprF -> TyCheckSt -> TyCheckM () -> IO (Either TCError TyTree)
typeCheck tree initSt setup =
  catch
    (ambiguityCheck =<< observeAllT (evalStateT (runTyCheckM $ setup *> convertTree tree) initSt))
    (pure . Left)
  where
    ambiguityCheck [result] = pure $ Right result
    amiguityCheck (_ : _ : _) = throw AmbiguousTypes

noSetup :: TyCheckM ()
noSetup = pure ()

restoreBindings :: Map Text Binding -> TyCheckM ()
restoreBindings oldBindings =
  modify $ \st -> st { bindings = oldBindings }

convertTree :: LocTree RowCol ExprF -> TyCheckM TyTree
convertTree tree = AST.transform go tree
  where
    go :: RowCol -> RowCol -> ExprF (TyCheckM TyTree) -> TyCheckM (TyExprF TyTree)
    go _ _ (ProdF checkElems) = do
      elemTrees <- sequence checkElems
      let tys = tyF . locContent <$> elemTrees
      thisTy <- tyCell . Filled . RtProdF $ Vec.fromList tys
      pure $ TyExprF thisTy $ RtProdF $ Vec.fromList elemTrees
    go _ _ (LamF checkName checkBody) = do
      depth <- lambdaDepth <$> get
      inputTy <- tyCell Empty
      oldBindings <- assuming checkName $ LambdaBound inputTy depth
      bodyTree <- checkBody
      restoreBindings oldBindings
      pure $ TyExprF (tyF $ locContent bodyTree) (RtLamF bodyTree)
    go _ _ (AnnF checkTerm checkAnn) = do
      annTree <- checkAnn
      tyTerm <- tyCell $ Filled RtTyF
      unify (tyOf annTree) tyTerm <|> throw TypeMismatch
      termTree <- checkTerm
      annTerm <- tyCell $ Filled $ unwrapVal annTree
      unify (tyOf termTree) annTerm <|> throw TypeMismatch
      pure $ locContent termTree
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
extractTy tree = go $ tyF $ locContent tree
  where
    go :: TyCell -> IO RtVal
    go (TyCell tyCell) = do
      (thisCell, _, _, _) <- rootInfo tyCell
      readRef (value thisCell) >>= \case
        Filled (RtProdF xs) -> RtProd <$> traverse go xs
        Filled _ -> undefined
        Empty -> undefined

extractValue :: TyTree -> IO RtVal
extractValue tree =
  go . exprF $ locContent tree
  where
    go :: RtValF TyTree -> IO RtVal
    go (RtProdF xs) = RtProd <$> traverse (go . exprF . locContent) xs
    go _ = undefined
