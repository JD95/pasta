{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Eval where

import           Control.Monad.Reader
import           Data.Functor.Foldable
import           Numeric.Natural

import           Core
import           Subst
import           Env
import           Expr
import           Typed

class Functor f => Eval f where
  eval :: (SymLookup String (Fix f) m) => f (Fix f, m (Fix f)) -> m (Fix f)

instance (Eval f) => Eval (Expr Core f) where
  eval (App (_, x) (_, y)) = do
     y'   <- y
     func <- x
     case unfix func of
       Lam _ body -> pure $ subst y' (0 :: Natural) body
       _          -> error "Cannot reduce non lambda value!"

  -- Don't evaluate lambdas
  eval (Lam x (body, _)    ) = pure $ lam x body

  eval (Val (Bound  i     )) = pure $ var i
  eval (Val (Inline (_, x))) = x
  eval (Val (Free   name  )) = symLookup name >>= \case
    Nothing -> error "Variable was not in context"
    Just x  -> pure x
  eval (Expr x) = eval x


-- eval :: (SymLookup String (Fix CoreE) m) => Fix CoreE -> m (Fix CoreE)
-- eval = para (goExpr . unCoreE)
--  where

--   goExpr (App (_, x) (_, y)) = do
--     y'   <- y
--     func <- x
--     case unCoreE $ unfix func of
--       Lam _ body -> pure $ subst y' (0 :: Natural) body
--       _          -> error "Cannot reduce non lambda value!"

--   -- Don't evaluate lambdas
--   goExpr (Lam x (body, _)    ) = pure $ lam x body

--   goExpr (Val (Bound  i     )) = pure $ var i
--   goExpr (Val (Inline (_, x))) = x
--   goExpr (Val (Free   name  )) = symLookup name >>= \case
--     Nothing -> error "Variable was not in context"
--     Just x  -> pure x
--   goExpr (Expr x) = goType x

--   goType (RArr _ (_, output)              ) = rig () <$> output
--   goType (PArr _ (_, output)              ) = pol () <$> output
--   goType (TArr opts (_, input) (_, output)) = arrow opts <$> input <*> output
--   goType (TCon  name                      ) = pure $ con name
--   goType (Typed _                         ) = undefined

