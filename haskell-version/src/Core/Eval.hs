{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Eval where

import           Control.Monad.Reader
import           Data.Functor.Foldable

import           Core
import           Core.Subst
import           Env
import           Expr
import           Typed

eval :: (SymLookup String (Fix CoreE) m) => Fix CoreE -> m (Fix CoreE)
eval = para (goExpr . unCoreE)
 where

  goExpr (App (_, x) (_, y)) = do
    y'   <- y
    func <- x
    case unCoreE $ unfix func of
      Lam _ body -> pure $ runReader (subst body) (y', 0)
      _          -> error "Cannot reduce non lambda value!"

  -- Don't evaluate lambdas
  goExpr (Lam x (body, _)    ) = pure $ lam x body

  goExpr (Val (Bound  i     )) = pure $ var i
  goExpr (Val (Inline (_, x))) = x
  goExpr (Val (Free   name  )) = symLookup name >>= \case
    Nothing -> error "Variable was not in context"
    Just x  -> pure x
  goExpr (Expr x) = goType x

  goType (RArr _ (_, output)              ) = rig () <$> output
  goType (PArr _ (_, output)              ) = pol () <$> output
  goType (TArr opts (_, input) (_, output)) = arrow opts <$> input <*> output
  goType (TCon  name                      ) = pure $ con name
  goType (Typed _                         ) = undefined

