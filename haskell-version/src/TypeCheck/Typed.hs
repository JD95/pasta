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
import Data.Functor.Classes
import Data.Functor.Foldable hiding (project)
import Data.Hashable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sum
import Data.Text
import Data.Traversable
import Data.Vector (Vector)
import Data.Wedge
import Display
import Logic.Info
import Logic.Propagator
import Logic.Propagator.Class
import Logic.Propagator.PrimCell

data Ann a = Ann a a deriving (Eq, Show)

deriving instance Functor Ann

deriving instance Foldable Ann

deriving instance Traversable Ann

instance Eq1 Ann where
  liftEq f (Ann a b) (Ann c d) = f a c && f b d

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

newtype TypeCell m h = TypeCell {unTypeCell :: PrimCell m (Partial h)}

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

myZip ::
  Free f a ->
  Free f a ->
  Wedge
    (Free f a, a) -- | When a subtree matches a hole
    (Free f a)    -- | When both trees zip
myZip = _

class Zip f where
  zipF :: f a -> f b -> Maybe (f (a, b))

instance Apply Zip fs => Zip (Sum fs) where
  zipF x y = join $ apply2' @Zip (\inj a b -> inj <$> zipF a b) x y
