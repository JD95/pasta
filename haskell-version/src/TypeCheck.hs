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
import Control.Monad.State
import Data.IORef
import Data.Typeable
import qualified Data.Vector as Vec
import Lexer (RowCol)
import Runtime.Prop
import Runtime.Ref
import Runtime.Types

data TCheckEnv = TCheckEnv

newtype TyCheckM a = TyCheckM {runTyCheckM :: StateT () IO a}
  deriving (Functor, Applicative, Monad, MonadState (), MonadIO)

instance Ref TyCheckM IORef where
  newRef = liftIO . newIORef
  readRef = liftIO . readIORef
  writeRef r = liftIO . writeIORef r

data TCError = TCError
  deriving (Show)

instance Exception TCError where
  toException TCError = SomeException TCError
  fromException (SomeException e) = cast e
  displayException TCError = "Type Error!"

data TyCell = TyCell {unTyCell :: Cell TyCheckM IORef (Maybe (RtValF TyCell))}

data TyExpr a = TyExpr {ty :: TyCell, expr :: (ExprF a)}

typeCheck :: LocTree RowCol ExprF -> IO (Either TCError (LocTree RowCol TyExpr))
typeCheck tree =
  catch
    (Right <$> evalStateT (runTyCheckM (AST.transform go tree)) ())
    (pure . Left)
  where
    go :: RowCol -> RowCol -> ExprF (LocTree RowCol TyExpr) -> TyCheckM (TyExpr (LocTree RowCol TyExpr))
    go _ _ (ProdF xs) = do
      let tys = ty . locContent <$> xs
      thisCell <- cell (Just $ RtProdF (Vec.fromList tys))
      let thisTy = TyCell thisCell
      pure $ TyExpr thisTy (ProdF xs)

extractTy :: LocTree RowCol TyExpr -> IO Expr
extractTy tree = do
  evalStateT (runTyCheckM (go $ ty $ locContent tree)) ()
  where
    go :: TyCell -> TyCheckM Expr
    go (TyCell tyCell) = do
      readRef (value tyCell) >>= \case
        Just (RtProdF xs) -> Prod . Vec.toList <$> traverse go xs
        Just _ -> undefined
        Nothing -> undefined
