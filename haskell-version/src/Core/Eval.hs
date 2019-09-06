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

  go (Here layer) =
    let (ExprBuilder _ lam var _ _) = exprBuilder (Proxy @Core)
    in  case layer of
          (App (_, x) (_, y)) -> do
            y'   <- y
            func <- x
            case unfix func of
              (Here (Lam _ body)) -> pure $ subst y' (0 :: Natural) body
              _                   -> error "Cannot reduce non lambda value!"

          -- Don't evaluate lambdas
          (Lam x (body, _)    ) -> pure $ lam x body

          (Val (Bound  i     )) -> pure $ var i
          (Val (Inline (_, x))) -> x
          (Val (Free   name  )) -> symLookup name >>= \case
            Nothing -> error "Variable was not in context"
            Just x  -> pure x

  go (There (Here layer)) =
    let (TypeBuilder rig pol arrow con t) = typeBuilder (Proxy @Core)
    in  case layer of
          (RArr _ (_, output)              ) -> rig () <$> output
          (PArr _ (_, output)              ) -> pol () <$> output
          (TArr opts (_, input) (_, output)) -> arrow opts <$> input <*> output
          (TCon name                       ) -> pure $ con name
          (Type n                          ) -> pure $ t n

  go _ = undefined
