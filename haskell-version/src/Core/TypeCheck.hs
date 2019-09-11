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

module Core.TypeCheck where

import           Data.Functor.Identity
import           Data.Bifunctor
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

newtype Fill = MkFill { unFill :: String } deriving (Eq)

class (Functor f, Functor g) => Unify f g where
  unify :: (MonadThrow m) => f a -> g a -> m (Either Fill [(a, a)])

instance Unify (Expr Check) (Expr Check) where
  unify (Val (Bound i))(Val (Bound j)) = if i == j then pure . pure $ [] else throwM CantUnify

instance Unify (Typed Check) (Typed Check) where
  unify (RArr _ x)(RArr _ y) = pure . pure $ [(x,y)]

instance Unify Checked (Expr Check) where
  unify (Hole s) _ = pure $ Left (MkFill s)

instance Unify Checked (Typed Check) where
  unify (Hole s) _ = pure $ Left (MkFill s)

instance Unify Checked Checked where
  unify (Hole s) _ = pure $ Left (MkFill s)

instance Subst (Expr ix) Fill where
  depth = flip const

  getKey = const Nothing

instance Subst (Typed ix) Fill where
  depth = flip const
  getKey = const Nothing

instance Subst Checked Fill where
  depth _ n = n

  getKey (Hole s) = Just (MkFill s)

solveConstraints :: (MonadState Ctx m, MonadThrow m) => m ()
solveConstraints = do
  state (\(Ctx (w : ws) rs) -> (w, Ctx ws rs)) >>= \case
    Flat (EqC x y) -> applyUnify [(x, y)]
 where

  applyUnify
    :: (MonadState Ctx m, MonadThrow m) => [(Fix CheckE, Fix CheckE)] -> m ()
  applyUnify []                      = pure ()
  applyUnify ((Fix x, Fix y) : rest) = case (x, y) of
    -- Valid Cases
    (Here a, Here b) -> runUnify a b rest
    (There (Here a), There (Here b)) -> runUnify a b rest
    (There (There (Here a)), Here b) -> runUnify a b rest
    (There (There (Here a)), There (There (Here b))) -> runUnify a b rest
    (There (There (Here a)), There (Here b)) -> runUnify a b rest
    (Here a, There (There (Here b))) -> runUnify b a rest
    (There (Here a), There (There (Here b))) -> runUnify b a rest

    -- Invalid Cases
    (Here _, There (Here _)) -> error "Can't Unify an expr with a type term"
    (There (Here _), Here _) -> error "Can't Unify a type with an expr term"
    _ -> error "Invalid Unify"

  runUnify a b rest = do
    unify a b >>= \case
      Left fill -> do
        let b' = Fix . inj $ b
        -- Apply the substitution to all constraints
        modify (\(Ctx ws rs) -> Ctx ((fmap . fmap) (subst b' fill) ws) rs)
        -- Apply the subst to rest of unification nodes and continue solving
        let f = Fix . (fmap (subst b' fill) . unfix)
        applyUnify (bimap f f <$> rest)
      Right more -> applyUnify more *> applyUnify rest



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

check :: Fix CoreE -> Fix CoreE -> Either SomeException ()
check e goal =
  let (ty, (Ctx ws rs, _)) =
        runState (genConstraints e) (Ctx [] mempty, initNames)
  in  runIdentity $ runCatchT $ evalStateT solveConstraints
                                           (Ctx (ty ~: toCheck goal : ws) rs)

testCheck = do
  let (e :: Fix CoreE) = mkVar ce 0
  let (t :: Fix CoreE) = mkCon ce "Thing"
  print $ check e t
