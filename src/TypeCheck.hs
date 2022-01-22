{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeCheck where

import AST.Expr
import AST.LocTree
import qualified AST.LocTree as AST
import Control.Exception
import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text
import Data.Typeable
import qualified Data.Vector as Vec
import Lexer (RowCol)
import Runtime.Prop
import Runtime.Ref
import Runtime.Types

data TCheckEnv = TCheckEnv

data TyCheckSt = TyCheckSt
  { bindings :: Map Text TyCell
  }

defaultTyCheckSt :: TyCheckSt
defaultTyCheckSt = TyCheckSt {bindings = Map.empty}

newtype TyCheckM a = TyCheckM {runTyCheckM :: StateT TyCheckSt (LogicT IO) a}
  deriving (Functor, Applicative, Monad, MonadState TyCheckSt, MonadIO)

instance Ref TyCheckM IORef where
  newRef = liftIO . newIORef
  readRef = liftIO . readIORef
  writeRef r = liftIO . writeIORef r

data TCError
  = TypeMismatch
  | AmbiguousTypes
  deriving (Show)

instance Exception TCError where
  toException e = SomeException e
  fromException (SomeException e) = cast e
  displayException TypeMismatch = "Type Mistmatch!"
  displayException AmbiguousTypes = "Ambiguous Types!"

data TyCell = TyCell {unTyCell :: Cell TyCheckM IORef (Maybe (RtValF TyCell))}

data TyExpr a = TyExpr {ty :: TyCell, expr :: (ExprF a)}

typeCheck :: LocTree RowCol ExprF -> TyCheckSt -> IO (Either TCError (LocTree RowCol TyExpr))
typeCheck tree initSt =
  catch
    (ambiguityCheck =<< observeAllT (evalStateT (runTyCheckM (AST.transform go tree)) initSt))
    (pure . Left)
  where
    ambiguityCheck [result] = pure $ Right result
    amiguityCheck (_ : _ : _) = throw AmbiguousTypes

    go :: RowCol -> RowCol -> ExprF (TyCheckM (LocTree RowCol TyExpr)) -> TyCheckM (TyExpr (LocTree RowCol TyExpr))
    go _ _ (ProdF checkElems) = do
      xs <- sequence checkElems
      let tys = ty . locContent <$> xs
      thisCell <- cell . Just . RtProdF $ Vec.fromList tys
      let thisTy = TyCell thisCell
      pure $ TyExpr thisTy (ProdF xs)
    go _ _ (SymbolF name) = do
      thisTy <- lookupTyFor name
      pure . TyExpr thisTy $ SymbolF name

lookupTyFor :: Text -> TyCheckM TyCell
lookupTyFor name = do
  table <- bindings <$> get
  case Map.lookup name table of
    Just result -> pure result
    Nothing -> error "No type for symbol"

extractTy :: LocTree RowCol TyExpr -> IO Expr
extractTy tree = go $ ty $ locContent tree
  where
    go :: TyCell -> IO Expr
    go (TyCell tyCell) = do
      readRef (value tyCell) >>= \case
        Just (RtProdF xs) -> Prod . Vec.toList <$> traverse go xs
        Just _ -> undefined
        Nothing -> undefined

extractValue :: LocTree RowCol TyExpr -> IO RtVal
extractValue tree =
  go . expr $ locContent tree
  where
    go :: ExprF (LocTree RowCol TyExpr) -> IO RtVal
    go (ProdF xs) = RtProd . Vec.fromList <$> traverse (go . expr . locContent) xs
    go _ = undefined
