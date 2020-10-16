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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeCheck.Check where

import AST.Core
import AST.Transform
import Control.Applicative
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
import Data.Functor.Foldable (Fix (..), cata)
import Data.Hashable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sum
import Data.Text (Text, pack)
import Data.Traversable
import Data.Vector (Vector)
import Data.Wedge
import Display
import Logic
import Logic.Info
import Logic.Propagator
import Logic.Propagator.Class
import Logic.Propagator.PrimCell
import Text.Show.Deriving
import TypeCheck.Typed
import Unique

newtype TypeMerge = MkTypeMerge {unTypeMerge :: Partial Hole} deriving (Eq, Show)

newtype TypeCell m = TypeCell {unTypeCell :: PrimCell m TypeMerge}

instance Merge TypeMerge where
  merge (OldInfo (MkTypeMerge old)) (NewInfo (MkTypeMerge new)) =
    diffToInfo $ MkTypeMerge <$> diff (\x y -> if x == y then Same x else Update y) old new

  -- case (unTypeMerge old, unTypeMerge new) of
  --   (Free x, Free y) ->
  --     diffToInfo $
  --       MkTypeMerge . Free
  --         <$> diff (\x y -> if x == y then Same x else Update y) x y
  --   (Free x, Pure _) -> Info . MkTypeMerge $ Free x
  --   (Pure _, Free x) -> Info . MkTypeMerge $ Free x
  --   (Pure x, Pure y) ->
  --     if x /= y
  --       then -- must use new info else infinite loop
  --         Info . MkTypeMerge $ Pure y
  --       else NoInfo

  isTop = foldr (&&) True . fmap (const False) . unTypeMerge

data CheckST m = CheckST {symbols :: Map Text (TypeCell m)}

initCheckST :: CheckST m
initCheckST = CheckST mempty

lookupSymbol :: MonadState (CheckST m) m => Text -> m (Maybe (TypeCell m))
lookupSymbol t = Map.lookup t . symbols <$> get

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
    f (m (TypeCell m)) ->
    m (TypeCell m)

instance TypeCheck f => TypeCheck (F.FreeF f Hole) where
  check (F.Free f) = check f
  check (F.Pure x) = pure . TypeCell =<< newPrimCell

instance Apply TypeCheck fs => TypeCheck (Sum fs) where
  check = apply @TypeCheck check

instance TypeCheck App where
  check (App getFunc getInput) = do
    func <- unTypeCell <$> getFunc
    input <- unTypeCell <$> getInput
    output <- newPrimCell

    holeA <- newHole
    holeB <- newHole

    inform (Info . MkTypeMerge $ intTy -:> intTy) func

    -- (a,b) ~ (a -> b)
    funcTy <- newPrimCell @_ @(TypeMerge, TypeMerge)

    -- func ~ (a -> b) ==> funcTy ~ (a, b)
    waitOn [SomeCell func] $ do
      flip propagate [funcTy] $ do
        unTypeMerge <$> content' func >>= \case
          Free x -> case project x of
            Just (Arr a b) -> do
              pure $ Info (MkTypeMerge a, MkTypeMerge b)
            _ -> pure Contradiction
          Pure h -> pure NoInfo

    waitOn [SomeCell funcTy] $ do
      flip propagate [func] $ do
        (MkTypeMerge a, MkTypeMerge b) <- content' funcTy
        pure . Info . MkTypeMerge $ a -:> b

    waitOn [SomeCell funcTy] $ do
      flip propagate [output] $ do
        (_, b) <- content' funcTy
        pure . Info $ b

    waitOn [SomeCell input, SomeCell output] $ do
      flip propagate [funcTy] $ do
        a <- content' input
        b <- content' output
        pure . Info $ (a, b)

    pure $ TypeCell output

instance TypeCheck Prim where
  check (PInt _) = do
    ty <- newPrimCell
    inform (Info . MkTypeMerge . Free . inject $ IntTy) ty
    pure . TypeCell $ ty

instance TypeCheck Data where
  check = undefined

instance TypeCheck Lam where
  check (Lam input getBody) = do
    body <- unTypeCell <$> getBody
    inputTy <- unTypeCell <$> existingOrNewCell input
    lamType <- newPrimCell @_ @(TypeMerge, TypeMerge)
    ty <- newPrimCell

    holeA <- newHole
    holeB <- newHole
    inform (Info . MkTypeMerge $ holeA -:> holeB) ty

    -- (inputTy ~ a) /\ (body ~ b) ==> lamType ~ (a,b)
    waitOn [SomeCell inputTy, SomeCell body] $ do
      flip propagate [lamType] $ do
        a <- content' inputTy
        b <- content' body
        pure $ Info (a, b)

    -- lamType ~ (a,b) ==> inputTy ~ a
    waitOn [SomeCell lamType] $ do
      flip propagate [inputTy] $ do
        (a, _) <- content' lamType
        pure $ Info a

    -- lamType ~ (a,b) ==> body ~ b
    waitOn [SomeCell lamType] $ do
      flip propagate [body] $ do
        (_, b) <- content' lamType
        pure $ Info b

    -- lamType ~ (a,b) ==> ty ~ (a -> b)
    waitOn [SomeCell lamType] $ do
      flip propagate [ty] $ do
        (MkTypeMerge a, MkTypeMerge b) <- content' lamType
        pure . Info . MkTypeMerge $ a -:> b

    -- ty ~ (a -> b) ==> lamType ~ (a,b)
    waitOn [SomeCell ty] $ do
      flip propagate [lamType] $ do
        unTypeMerge <$> content' ty >>= \case
          Free x -> case project x of
            Just (Arr a b) -> pure $ Info (MkTypeMerge a, MkTypeMerge b)
            _ -> pure Contradiction
          Pure h -> pure NoInfo

    pure $ TypeCell ty

instance TypeCheck FreeVar where
  check (FreeVar name) = existingOrNewCell name

instance TypeCheck Ann where
  check = undefined

runTypeCheck :: CheckST TypeT -> Partial Hole -> IO [Info TypeMerge]
runTypeCheck st t = observeAllT . flip evalStateT st . runTypeT $ result
  where
    result :: TypeT (Info TypeMerge)
    result = do
      TypeCell r <- cata check t
      x <- content r
      pure x

test :: IO ()
test = do
  let term = lam "x" (free "x") `app` int 0 :: Partial Hole
  a <- newHole
  let st = initCheckST
  runTypeCheck st term >>= \case
    (Info (MkTypeMerge result) : _) ->
      -- TODO: Figure out a way to test for alpha equivalence
      print $ renderHoles result
    _ -> undefined
