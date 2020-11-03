{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TypeCheck.Typed.Check where

import AST.Core
import AST.Transform
import Control.Applicative
import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Free
import qualified Control.Monad.Free as Free
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.State.Strict (MonadState (..), StateT, evalStateT)
import qualified Control.Monad.State.Strict as ST
import qualified Control.Monad.Trans.Free as F
import Data.Eq.Deriving
import Data.Foldable
import Data.Functor.Classes
import Data.Functor.Foldable (Fix (..), cata, para)
import Data.Hashable
import Data.Maybe
import Data.Sum
import Data.Text (Text, pack)
import qualified Data.Text.IO as Text
import Data.Traversable
import Data.Vector (Vector)
import Data.Wedge
import Display
import Logic
import Logic.Info
import Logic.Propagator
import Logic.Propagator.Class
import Logic.Propagator.PrimCell
import Numeric.Natural
import RIO hiding (Data, display)
import RIO.Map (Map)
import qualified RIO.Map as Map
import Text.Show.Deriving
import TypeCheck.Typed.Eval
import TypeCheck.Typed.Render
import TypeCheck.Typed.Stages
import Unique
import Prelude hiding (pi)

data Source = IsExpected | Learned deriving (Eq, Show)

learn :: PosInfo -> Partial Hole -> Info TypeMerge
learn p = Info . MkTypeMerge p Learned

expected :: PosInfo -> Partial Hole -> Info TypeMerge
expected p = Info . MkTypeMerge p IsExpected

instance Semigroup Source where
  Learned <> _ = Learned
  _ <> Learned = Learned
  _ <> _ = IsExpected

data TypeMerge = MkTypeMerge {pos :: PosInfo, source :: Source, unTypeMerge :: Partial Hole} deriving (Eq, Show)

newtype TypeCell m = TypeCell {unTypeCell :: PrimCell m TypeMerge}

isErr :: Partial Hole -> Maybe (Err (Partial Hole))
isErr (Free x) = project x
isErr _ = Nothing

instance Merge TypeMerge where
  merge (OldInfo (MkTypeMerge oldPos oldSource old)) (NewInfo (MkTypeMerge newPos newSource new)) =
    case (isErr old, isErr new) of
      (Just xs, Just ys) ->
        if xs /= ys then learn oldPos $ errs [xs, ys] else NoInfo
      (Just x, Nothing) -> NoInfo
      (Nothing, Just x) -> learn newPos new
      (Nothing, Nothing) ->
        let diffHoles x y = if x == y then Same x else Update y
            result = diff diffHoles old new
         in case result of
              Conflict -> learn mempty $
                case (oldSource, newSource) of
                  (Learned, Learned) -> conflict (oldPos <> newPos) old new
                  (Learned, IsExpected) -> mismatch oldPos (Expected new) (Given old)
                  (IsExpected, Learned) -> mismatch newPos (Expected old) (Given new)
                  (IsExpected, IsExpected) -> conflict (oldPos <> newPos) old new
              _ -> diffToInfo $ MkTypeMerge (oldPos <> newPos) (oldSource <> newSource) <$> result

  isTop = foldr (&&) True . fmap (const False) . unTypeMerge

data CheckST m = CheckST {symbols :: Map Text (TypeCell m)}

initCheckST :: CheckST m
initCheckST = CheckST mempty

lookupSymbol :: MonadState (CheckST m) m => Text -> m (Maybe (TypeCell m))
lookupSymbol t = Map.lookup t . symbols <$> get

removeSymbol :: MonadState (CheckST m) m => Text -> m ()
removeSymbol k = do
  ST.modify $ \st -> st {symbols = Map.delete k (symbols st)}

addSymbol :: MonadState (CheckST m) m => Text -> TypeCell m -> m ()
addSymbol k v = do
  ST.modify $ \st -> st {symbols = Map.alter (const $ Just v) k (symbols st)}

existingOrNewCell :: (Network m, PrimMonad m, MonadState (CheckST m) m) => Text -> m (TypeCell m)
existingOrNewCell t =
  lookupSymbol t >>= \case
    Just val -> pure val
    Nothing -> do
      x <- TypeCell <$> newPrimCell
      addSymbol t x
      pure x

withBound :: MonadState (CheckST m) m => Text -> TypeCell m -> m a -> m a
withBound t newTy action = do
  restore <-
    lookupSymbol t >>= \case
      Just oldTy -> pure $ addSymbol t oldTy
      Nothing -> pure $ removeSymbol t
  addSymbol t newTy
  result <- action
  restore
  pure result

newtype TypeT a = TypeT
  {runTypeT :: StateT (CheckST TypeT) (LogicT IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      Alternative,
      MonadPlus,
      MonadState (CheckST TypeT)
    )

instance PrimMonad TypeT where
  type PrimState TypeT = PrimState IO
  primitive f = liftIO $ primitive f

instance Network TypeT where
  type NetworkId TypeT = UniqueM TypeT

  newId = liftIO newId

  alert = id

  propagate action targets = do
    action >>= \case
      NoInfo -> pure ()
      Info x -> forM_ targets $ inform (Info x)
      Contradiction -> empty

  solve = pure ()

newHole :: PrimMonad m => m (Partial Hole)
newHole = do
  Unique i _ <- newUnique
  pure $ hole i

class TypeCheck f where
  check ::
    ( Network m,
      MonadIO m,
      PrimMonad m,
      MonadState (CheckST m) m
    ) =>
    PosInfo ->
    f (Partial Hole, m (TypeCell m)) ->
    m (TypeCell m)

instance TypeCheck f => TypeCheck (F.FreeF f Hole) where
  check env (F.Free f) = check env f
  check _ (F.Pure x) = pure . TypeCell =<< newPrimCell

instance Apply TypeCheck fs => TypeCheck (Sum fs) where
  check env = apply @TypeCheck (check env)

instance TypeCheck Err where
  check _ (Errs xs) = do
    result <- newPrimCell
    inform (learn mempty $ errs $ fmap fst <$> xs) result
    pure $ TypeCell result
  check _ (Mismatch p (Expected (x, _)) (Given (y, _))) = do
    result <- newPrimCell
    inform (learn p $ mismatch p (Expected x) (Given y)) result
    pure $ TypeCell result

displayContent :: (MonadIO m, Network m, PrimMonad m) => PrimCell m TypeMerge -> m ()
displayContent cell = liftIO . print . fmap (display . renderHoles . unTypeMerge) =<< content cell

instance TypeCheck App where
  check pos (App (_, getFuncTy) (input, getInputTy)) = do
    funcTy <- unTypeCell <$> getFuncTy
    inputTy <- unTypeCell <$> getInputTy
    outputTy <- newPrimCell

    holeA <- newHole
    holeB <- newHole

    inform (expected pos $ holeA -:> holeB) funcTy

    -- funcTy ~ ((x : a) -> b) ==> output ~ b [x/input]
    waitOn [SomeCell funcTy] $ do
      unTypeMerge <$> content' funcTy >>= \case
        Free x -> case project x of
          Just (Arr s a b) -> do
            flip propagate [inputTy] $ pure . expected pos $ a
            flip propagate [outputTy] $ do
              pure . learn mempty $ case s of
                Just name -> para (tySubst name input) b
                Nothing -> b
          _ -> case isErr (Free x) of
            Just _ -> do
              flip propagate [outputTy] $ pure . learn mempty $ Free x
        Pure h -> pure ()

    -- (inputTy ~ a) ==> funcTy ~ a -> _
    waitOn [SomeCell inputTy] $ do
      flip propagate [funcTy] $ do
        a <- unTypeMerge <$> content' inputTy
        case isErr a of
          Just _ -> pure . learn mempty $ a
          Nothing -> do
            outHole <- newHole
            pure . expected pos $ a -:> outHole

    pure $ TypeCell outputTy

instance TypeCheck Prim where
  check pos (Arr name' (depIn, getDepInTy) (_, getOutTy)) = do
    depInTy <- unTypeCell <$> getDepInTy
    outTy <-
      unTypeCell <$> case name' of
        Just name -> do
          depInCell <- newPrimCell
          inform (expected pos depIn) depInCell
          withBound name (TypeCell depInCell) getOutTy
        Nothing -> getOutTy
    result <- newPrimCell
    inform (learn pos $ ty 0) result
    pure (TypeCell result)
  check pos (Type n) = do
    result <- newPrimCell
    inform (learn pos . ty $ n + 1) result
    pure $ TypeCell result
  check pos IntTy = do
    result <- newPrimCell
    inform (learn pos $ ty 0) result
    pure $ TypeCell result
  check pos NatTy = do
    result <- newPrimCell
    inform (learn pos $ ty 0) result
    pure $ TypeCell result
  check pos (PInt _) = do
    result <- newPrimCell
    inform (learn pos $ intTy) result
    pure $ TypeCell result

instance TypeCheck Data where
  check _ _ = undefined

instance TypeCheck Lam where
  check pos (Lam input (_, getBodyTy)) = do
    bodyTy <- unTypeCell <$> getBodyTy
    inputTy <- unTypeCell <$> existingOrNewCell input
    ty <- newPrimCell

    holeA <- newHole
    holeB <- newHole
    inform (learn pos $ holeA -:> holeB) ty

    -- (inputTy ~ a) /\ (bodyTy ~ b) ==> lamType ~ (a,b)
    waitOn [SomeCell inputTy, SomeCell bodyTy] $ do
      flip propagate [ty] $ do
        a <- unTypeMerge <$> content' inputTy
        b <- unTypeMerge <$> content' bodyTy
        pure $ learn pos $ a -:> b

    -- ty ~ (a -> b) ==> inputTy ~ a /\ bodyTy ~ b
    waitOn [SomeCell ty] $ do
      unTypeMerge <$> content' ty >>= \case
        Free x -> case project x of
          Just (Arr _ a b) -> do
            flip propagate [inputTy] . pure $ expected pos a
            flip propagate [bodyTy] . pure $ expected pos b
          _ -> pure ()
        Pure h -> pure ()

    pure $ TypeCell ty

instance TypeCheck FreeVar where
  check _ (FreeVar name) = existingOrNewCell name

instance TypeCheck Ann where
  check pos (Ann (_, getTermTy) (givenTy, getT)) = do
    termTy <- unTypeCell <$> getTermTy
    t <- unTypeCell <$> getT
    inform (expected pos $ ty 0) t
    -- termTy ~ givenTy
    inform (expected pos givenTy) termTy
    pure $ TypeCell termTy

runTypeCheck :: CheckST TypeT -> Cofree Typed PosInfo -> IO [Info TypeMerge]
runTypeCheck st t = observeAllT . flip evalStateT st . runTypeT $ result
  where
    result :: TypeT (Info TypeMerge)
    result = do
      TypeCell r <- foo check t
      x <- content r
      pure x

-- | A recursion scheme that provides the tags from a Cofree structure
-- along with the original untagged structure and the result from
-- that branch. Essentially a paramorphism over a Cofree but with the
-- values added as an extra input instead of pattern matched out of the
-- Cofree structure.
foo :: (Monad m, Traversable f, Functor f) => (a -> f (Free f x, m b) -> m b) -> Cofree f a -> m b
foo go (env :< f) = do
  go env $
    fmap
      ( \x ->
          ( -- It would be nice if I could deconstruct
            -- the Cofree structure into Free only once.
            -- This makes using the original structure
            -- more expensive than it should be.
            uncofree x,
            foo go x
          )
      )
      f
