{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeCheck.Typed where

import AST.Core
import AST.Transform
import Control.Applicative
import Control.Monad
import Control.Monad.Free
import qualified Control.Monad.Free as Free
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Primitive
import qualified Control.Monad.Trans.Free as F
import Data.Eq.Deriving
import Data.Foldable
import Data.Functor.Classes
import Data.Functor.Foldable (Fix (..))
import Data.Hashable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sum
import Data.Text (Text, pack)
import Data.Traversable
import Data.Vector (Vector)
import Data.Wedge
import Display
import Logic.Info
import Logic.Propagator
import Logic.Propagator.Class
import Logic.Propagator.PrimCell
import Text.Show.Deriving

data Ann a = Ann a a deriving (Eq, Show)

deriving instance Functor Ann

deriving instance Foldable Ann

deriving instance Traversable Ann

instance Diffable Ann where
  diff f (Ann _ _) (Ann _ _) = undefined

deriveEq1 ''Ann
deriveShow1 ''Ann

ann :: (Ann :< fs) => Free (Sum fs) a -> Free (Sum fs) a -> Free (Sum fs) a
ann val = Free . inject . Ann val

newtype Hole = Hole {unHole :: Int} deriving (Eq, Show)

instance Display Ann where
  displayF (Ann x y) = x <> " : " <> y

hole :: Functor f => Int -> Free f Hole
hole i = Pure $ Hole i

type Partial h = Free Typed h

-- | Expressions with Types and free variables
type Typed = Sum [Prim, Data, App, Lam, FreeVar, Ann]

data RenderST = RenderST {idMap :: IntMap Text, names :: [String]}

newRenderST :: RenderST
newRenderST = RenderST mempty ns
  where
    ns = Prelude.concat $ iterate (Prelude.zipWith (<>) start) start
      where
        start = (: []) <$> ['a' .. 'z']

lookupName :: Members '[State RenderST] es => Int -> Eff es (Maybe Text)
lookupName i = IntMap.lookup i . idMap <$> get

addId :: Members '[State RenderST] es => Int -> Text -> Eff es ()
addId i t = modify (\st -> st {idMap = IntMap.alter (const . Just $ t) i (idMap st)})

popNextName :: Members '[State RenderST] es => Eff es Text
popNextName = do
  name <- (pack . Prelude.head . names) <$> get
  modify (\st -> st {names = Prelude.tail $ names st})
  pure name

renderHoles :: Partial Hole -> Fix Typed
renderHoles = asFix . run . evalState newRenderST . traverse go
  where
    go :: Members '[State RenderST] es => Hole -> Eff es (Fix Typed)
    go h =
      lookupName (unHole h) >>= \case
        Just name -> pure . asFix $ free name
        Nothing -> do
          newName <- popNextName
          addId (unHole h) newName
          pure . asFix $ free newName

class Zip f where
  zipF :: f a -> f b -> Maybe (f (a, b))

instance Apply Zip fs => Zip (Sum fs) where
  zipF x y = join $ apply2' @Zip (\inj a b -> inj <$> zipF a b) x y

newtype TypeMerge = MkTypeMerge {unTypeMerge :: Partial Hole} deriving (Eq)

newtype TypeCell m = TypeCell {unTypeCell :: PrimCell m TypeMerge}

instance Merge TypeMerge where
  merge (OldInfo old) (NewInfo new) =
    case (unTypeMerge old, unTypeMerge new) of
      (Free x, Free y) ->
        diffToInfo $
          MkTypeMerge . Free
            <$> diff (\x y -> if x == y then Same x else Update y) x y
      (Free x, Pure _) -> Info . MkTypeMerge $ Free x
      (Pure _, Free x) -> Info . MkTypeMerge $ Free x
      (Pure x, _) -> Info . MkTypeMerge $ Pure x

  isTop = foldr (&&) True . fmap (const False) . unTypeMerge

class TypeCheck f where
  check :: (Network m, PrimMonad m) => f (m (TypeCell m)) -> m (TypeCell m)

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

    -- (a,b) ~ (a -> b)
    funcTy <- newPrimCell @_ @(TypeMerge, TypeMerge)

    -- func ~ (a -> b) ==> funcTy ~ (a, b)
    waitOn [SomeCell func] $ do
      flip propagate [funcTy] $ do
        unTypeMerge <$> content' func >>= \case
          Free x -> case project x of
            Just (Arr a b) -> pure $ Info (MkTypeMerge a, MkTypeMerge b)
            _ -> pure Contradiction
          Pure h -> pure NoInfo

    -- (input ~ a) /\ (output ~ b) ==> funcTy ~ (a, b)
    waitOn [SomeCell input, SomeCell output] $ do
      flip propagate [funcTy] $ do
        x <- content' input
        y <- content' output
        pure . Info $ (x, y)

    -- funcTy ~ (a, b) ==> input ~ a
    waitOn [SomeCell funcTy] $ do
      flip propagate [input] $ do
        (a, _) <- content' funcTy
        pure . Info $ a

    -- funcTy ~ (a, b) ==> output ~ b
    waitOn [SomeCell funcTy] $ do
      flip propagate [output] $ do
        (_, b) <- content' funcTy
        pure . Info $ b

    pure $ TypeCell output

instance TypeCheck Prim where
  check (PInt _) = do
    ty <- newPrimCell
    inform (Info . MkTypeMerge . Free . inject $ IntTy) ty
    pure . TypeCell $ ty

instance TypeCheck Data where
  check = undefined

instance TypeCheck Lam where
  check = undefined

instance TypeCheck FreeVar where
  check = undefined

instance TypeCheck Ann where
  check = undefined
