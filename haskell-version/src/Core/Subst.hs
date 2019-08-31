{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Subst where

import           Control.Arrow hiding (app)
import           Control.Monad.Reader
import           Data.Functor.Foldable
import           Numeric.Natural

import           Expr
import           Typed
import           Core

subst :: (MonadReader (Fix CoreE, Natural) m) => Fix CoreE -> m (Fix CoreE)
subst = cata (goExpr . unCoreE) where

  incDepth = second (1+)
   
  goExpr (App x y) = app <$> x <*> y
  goExpr (Lam x body)     = do
     local incDepth (lam x <$> body)
  goExpr (Val  (Bound  i))   = do
    (sub, depth) <- ask
    pure $ if i == depth
      then sub
      else var i

  goExpr (Val  (Inline x))      = x
  goExpr (Val  (Free   name))   = pure $ free name 
  goExpr (Expr x         )      = goType x

  goType (RArr x output) = rig x <$> output
  goType (PArr x output) = pol x <$> output
  goType (TArr opts input output) = arrow opts <$> input <*> output
  goType (TCon  name) = pure $ con name
  goType (Typed _   ) = undefined
