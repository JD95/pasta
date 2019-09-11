{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.TypeCheck.Constrain where

import           Control.Monad.State.Strict
import           Data.Functor.Foldable
import qualified Data.Map.Merge.Strict         as Map
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

import           Constraint
import           Core
import           Env
import           Expr
import           Typed
import           Summable
import           Core.TypeCheck.Check

newtype Rigs = Rigs (Map String Rig)

instance Semigroup Rigs where
 (Rigs x) <> (Rigs y) = Rigs $
   Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (const (<>))) x y

instance Monoid Rigs where
  mempty = Rigs mempty

data Ctx = Ctx ([W (Fix CheckE)]) Rigs

instance Semigroup Ctx where
  (Ctx a b) <> (Ctx c d) = Ctx (a <> c) (b <> d)

instance Monoid Ctx where
  mempty = Ctx mempty mempty

class Monad m => ConstraintGen m where
  require :: W (Fix CheckE) -> m ()
  usage :: String -> Rig -> m ()

instance Monad m => ConstraintGen (StateT (Ctx, Names) m) where
  require w = modify $ \(Ctx ws rs, ns) -> (Ctx (w:ws) rs, ns)
  usage s r = modify $ \(Ctx ws (Rigs rs), ns) ->
    let f Nothing = Just r
        f (Just r') = Just (r <> r')
    in (Ctx ws (Rigs $ Map.alter f s rs), ns)

data Names = Next String Names

initNames = go (zipWith (\l n -> show l <> show n) (cycle ['a' .. 'z']) [1 ..])
  where go (x : xs) = Next x (go xs)

instance Monad m => NameGen (StateT (Ctx, Names) m) where
  newName = do
    (ctx, Next x xs) <- get
    put (ctx, xs)
    pure x

genConstraints :: (ConstraintGen m, NameGen m) => Fix CoreE -> m (Fix CheckE)
genConstraints = cata go
 where
  go (Here layer) = case layer of

    (Val (Bound  _   )) -> hole <$> newName
    (Val (Free   name)) -> pure $ undefined
    (Val (Inline x   )) -> x

    (App x y          ) -> do
      xTy       <- x
      yTy       <- y
      inTy      <- hole <$> newName
      outTy     <- hole <$> newName
      -- I need some way to generate constraints for the
      -- arrow opts. In other words, how to constrain rig
      -- and pol values?
      --
      -- Rig values need usage counts
      -- Pol values need inital laziness or strictness info from functions
      funRig    <- hole <$> newName
      funInPol  <- hole <$> newName
      funOutPol <- hole <$> newName
      let opts = (Right funRig, Right funInPol, Right funOutPol)
      require $ yTy ~: inTy
      require $ xTy ~: mkArrow cke opts inTy outTy
      require $ isRig funRig
      require $ isPol funInPol
      require $ isPol funOutPol
      pure outTy
    (Lam _ body) -> error "genConstraints lam not implemented"

  go (There (Here layer)) = case layer of
    (RArr _ _  ) -> error "genConstraints rig not implemented"
    (PArr _ _  ) -> error "genConstraints pol not implemented"
    (TArr _ _ _) -> error "genConstraints arr not implemented"
    (TCon _    ) -> error "genConstraints con not implemented"
    (Type _    ) -> error "genConstraints type not implemented"

  go _ = undefined
