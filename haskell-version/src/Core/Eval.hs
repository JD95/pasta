{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Core.Eval where

import           Data.Functor.Foldable
import           Numeric.Natural
import           Data.Proxy

import           Core
import           Subst
import           Env
import           Expr
import           Typed
import           Summable

eval :: (SymLookup String (Fix CoreE) m) => Fix CoreE -> m (Fix CoreE)
eval = para go
 where

  go (Here layer) = case layer of
    (App (_, x) (_, y)) -> do
      y'   <- y
      func <- x
      case unfix func of
        (Here (Lam _ body)) -> pure $ subst y' (0 :: Natural) body
        _                   -> error "Cannot reduce non lambda value!"

    -- Don't evaluate lambdas
    (Lam x (body, _)    ) -> pure $ mkLam ce x body

    (Val (Bound  i     )) -> pure $ mkVar ce i
    (Val (Inline (_, x))) -> x
    (Val (Free   name  )) -> symLookup name >>= \case
      Nothing -> error "Variable was not in context"
      Just x  -> pure x

  go (There (Here layer)) = case layer of
    (RArr _ (_, output)              ) -> mkRig ce () <$> output
    (PArr _ (_, output)              ) -> mkPol ce () <$> output
    (TArr opts (_, input) (_, output)) -> mkArrow ce opts <$> input <*> output
    (TCon name                       ) -> pure $ mkCon ce name
    (Type n                          ) -> pure $ mkT ce n

  go _ = undefined
