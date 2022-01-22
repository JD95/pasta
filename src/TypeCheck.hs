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
import Data.Word
import Lexer (RowCol)
import Runtime.Prop
import Runtime.Ref
import Runtime.Types

data TCheckEnv = TCheckEnv

type TyTree = LocTree RowCol TyExpr

data TyCheckSt = TyCheckSt
  { bindings :: Map Text (TyExpr TyTree),
    lambdaDepth :: Word32
  }

defaultTyCheckSt :: TyCheckSt
defaultTyCheckSt =
  TyCheckSt
    { bindings = Map.empty,
      lambdaDepth = 0
    }

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

data TyExpr a = TyExpr {ty :: TyCell, expr :: (RtValF a)}

assuming :: Text -> TyExpr TyTree -> TyCheckM ()
assuming name ty = modify $ \st -> st {bindings = Map.insert name ty (bindings st)}

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

convertTree :: LocTree RowCol ExprF -> TyCheckM TyTree
convertTree tree = AST.transform go tree
  where
    go :: RowCol -> RowCol -> ExprF (TyCheckM (LocTree RowCol TyExpr)) -> TyCheckM (TyExpr (LocTree RowCol TyExpr))
    go _ _ (ProdF checkElems) = do
      xs <- sequence checkElems
      let tys = ty . locContent <$> xs
      thisCell <- cell . Just . RtProdF $ Vec.fromList tys
      let thisTy = TyCell thisCell
      pure $ TyExpr thisTy $ RtProdF $ Vec.fromList xs
    go _ _ (SymbolF name) = lookupTyFor name

lookupTyFor :: Text -> TyCheckM (TyExpr (LocTree RowCol TyExpr))
lookupTyFor name = do
  table <- bindings <$> get
  case Map.lookup name table of
    Just result -> pure result
    Nothing -> error "No type for symbol"

extractTy :: LocTree RowCol TyExpr -> IO RtVal
extractTy tree = go $ ty $ locContent tree
  where
    go :: TyCell -> IO RtVal
    go (TyCell tyCell) = do
      readRef (value tyCell) >>= \case
        Just (RtProdF xs) -> RtProd <$> traverse go xs
        Just _ -> undefined
        Nothing -> undefined

extractValue :: LocTree RowCol TyExpr -> IO RtVal
extractValue tree =
  go . expr $ locContent tree
  where
    go :: RtValF (LocTree RowCol TyExpr) -> IO RtVal
    go (RtProdF xs) = RtProd <$> traverse (go . expr . locContent) xs
    go _ = undefined
