{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Core.Eval where

import           Data.Functor.Foldable
import qualified Data.Map.Strict               as Map
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
        (Here (Lam _ body    )) -> pure $ subst y' (0 :: Natural) body
        (Here (Proj (Index i))) -> case unfix y' of
          (Here (List _ xs)) -> if fromIntegral i < length xs
            then pure $ xs !! fromIntegral i
            else error "Index out of bounds!"
          _ -> error "@ expecting an index to access a product!"
        (Here (Proj (Key k))) -> case unfix y' of
          (Here (Record xs)) -> case Map.lookup k xs of
            Just val -> pure val
            Nothing  -> error "Field does not exist in record!"
          _ -> error "@ expecting a field to access a record"
        _ -> error "Cannot reduce non lambda value!"

    -- Don't evaluate lambdas
    (Lam x (body, _)    ) -> pure $ mkLam ce x body

    (Val (Bound  i     )) -> pure $ mkVar ce i
    (Val (Inline (_, x))) -> x
    (Val (Free   name  )) -> symLookup name >>= \case
      Nothing -> error "Variable was not in context"
      Just x  -> pure x
    (List t xs) -> do
      xs' <- traverse snd $ xs
      pure $ mkList ce t xs'
    (Record x) -> do
      xs' <- traverse snd $ x
      pure $ mkRec ce xs'
    (Inj i x  ) -> mkInj ce <$> traverse snd i <*> snd x
    (Proj i   ) -> mkProj ce <$> traverse snd i
    (Case x xs) -> do
      x' <- snd x
      case unfix x' of
        (Here (Inj (Index i) val)) -> case caseLookupIndex ce i xs of
          Nothing        -> error "Unmatched Pattern!"
          Just (_, body) -> pure $ subst val (0 :: Natural) (fst body)
        _ -> error "Value under case is not a sum type"


  (There (Here layer)) -> case layer of
    (RArr _ (_, output)              ) -> mkRig ce () <$> output
    (PArr _ (_, output)              ) -> mkPol ce () <$> output
    (TArr opts (_, input) (_, output)) -> mkArrow ce opts <$> input <*> output
    (TCon name                       ) -> pure $ mkCon ce name
    (NewType name ty                 ) -> mkNewType ce name <$> snd ty
    (Type n                          ) -> pure $ mkT ce n

  _ -> undefined
