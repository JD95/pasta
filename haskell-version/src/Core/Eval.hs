{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Core.Eval where

import           Data.Functor.Foldable
import           Numeric.Natural
import           Polysemy

import           Core
import           Subst
import           Env
import           Expr
import           Typed
import           Summable

eval
  :: (Member (SymLookup String (Fix CoreE)) r) => Fix CoreE -> Sem r (Fix CoreE)
eval = para $ \case
  (Here layer) -> case layer of
    (App (_, x) (_, y)) -> do
      y'   <- y
      func <- x
      case unfix func of
        (Here (Lam _ body)) -> pure $ subst y' (0 :: Natural) body
        (Here (Proj i)) -> case unfix y' of
          (Here (List xs)) ->
            if fromIntegral i < length xs
              then pure $ xs !! fromIntegral i
              else error "Index out of bounds!"
          _ -> error "@ must be used with a list!"
        _                   -> error "Cannot reduce non lambda value!"

    -- Don't evaluate lambdas
    (Lam x (body, _)    ) -> pure $ mkLam ce x body

    (Val (Bound  i     )) -> pure $ mkVar ce i
    (Val (Inline (_, x))) -> x
    (Val (Free   name  )) -> symLookup name >>= \case
      Nothing -> error "Variable was not in context"
      Just x  -> pure x
    (List xs) -> do
      xs' <- sequence . fmap snd $ xs
      pure $ mkList ce xs' 
    (Inj i x) -> mkInj ce i <$> snd x
    (Proj i) -> pure $ mkProj ce i 

  (There (Here layer)) -> case layer of
    (RArr _ (_, output)              ) -> mkRig ce () <$> output
    (PArr _ (_, output)              ) -> mkPol ce () <$> output
    (TArr opts (_, input) (_, output)) -> mkArrow ce opts <$> input <*> output
    (TCon name                       ) -> pure $ mkCon ce name
    (Type n                          ) -> pure $ mkT ce n

  _ -> undefined
