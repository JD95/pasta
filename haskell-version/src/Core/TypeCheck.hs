{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.TypeCheck where

import           Control.Comonad.Cofree
import           Control.Monad.Catch.Pure
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Functor.Const
import           Data.Functor.Foldable
import qualified Data.Map.Merge.Strict         as Map
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Void
import           Data.Proxy
import           Numeric.Natural

import           Constraint
import           Core
import           Subst
import           Env
import           Expr
import           Typed
import           Summable

data Checked a = Hole String deriving (Functor)

data TypeCheckError

type CheckE = Summed '[ Expr Check, Typed Check, Checked]

data Check

instance Expression Check where
  type LamOpts Check = LamOpts Core

instance TypedExpression Check where
  type RigName Check = RigName Core
  type PolName Check = RigName Core
  type ArrowOpts Check = ( Either (Abst Rig) (Fix CheckE)
                         , Either (Abst Pol) (Fix CheckE)
                         , Either (Abst Pol) (Fix CheckE)
                         )

cke = Proxy @Check

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

hole :: String -> Fix CheckE
hole = Fix . inj . Hole

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

data UnifyException = CantUnify deriving Show

instance Exception UnifyException

class Monad m => Unify m where
  addSubst :: String -> Fix CheckE -> m ()

unify :: (Unify m, MonadThrow m) => Fix CheckE -> Fix CheckE -> m (Fix CheckE)
unify = cata go
 where
  go (Here layer) = case layer of
    Val (Bound i) -> \case
      Fix (Here (Val (Bound j))) ->
        if i == j then pure $ mkVar cke i else throwM CantUnify
  go (There (Here layer)) = case layer of
    _ -> undefined
  go (There (There (Here layer))) = case layer of
    Hole s -> \y -> do
      addSubst s y
      pure y

  go _ = undefined

solveConstraints :: MonadThrow m => Ctx -> m ()
solveConstraints (Ctx ws rs) = error "solveConstraints not implemented"

toCheck :: Fix CoreE -> Fix CheckE
toCheck = cata go
 where
  go (Here layer) = case layer of
    Val (Bound  i) -> mkVar cke i
    Val (Free   x) -> mkFree cke x
    Val (Inline x) -> mkInline cke x
    App x y        -> mkApp cke x y
    Lam x body     -> mkLam cke x body
  go (There (Here layer)) = case layer of
    RArr x y           -> mkRig cke x y
    PArr x y           -> mkPol cke x y
    TArr (a, b, c) x y -> mkArrow cke (Left a, Left b, Left c) x y
    TCon x             -> mkCon cke x
    Type n             -> mkT cke n
  go _ = undefined

check :: (MonadThrow m) => Fix CoreE -> Fix CoreE -> m ()
check e goal = do
  let (ty, (Ctx ws rs, _)) =
        runState (genConstraints e) (Ctx [] mempty, initNames)
  solveConstraints (Ctx (ty ~: toCheck goal : ws) rs)
