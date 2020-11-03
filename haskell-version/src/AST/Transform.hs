{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AST.Transform where

import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Eq.Deriving
import Data.Functor.Compose
import Data.Functor.Foldable (Base (..), Fix (..), Recursive (..), cata)
import qualified Data.Functor.Foldable as Rec
import Data.Ord
import Data.Sum
import Logic.Info
import RIO
import Text.Show.Deriving

-- | Allows for unchanging parts of the AST to pass through a transform
pass :: (f :< g, Monad m, Traversable f) => f (m (Fix (Sum g))) -> m (Fix (Sum g))
pass = gpass id

-- | A generalized 'pass' given a natural transform from `Sum g` to `h`
gpass ::
  (f :< g, Monad m, Traversable f) =>
  (forall x. Sum g x -> h x) ->
  f (m (Fix h)) ->
  m (Fix h)
gpass into = fmap (Fix . into . inject) . sequence

asFix :: Functor f => Free f (Fix f) -> Fix f
asFix = iter Fix

asFree :: Functor f => Fix f -> Free f a
asFree = cata Free

-- | Represents the result of diffing two
-- structures.
data Diff a
  = -- | The structure was unchanged
    Same a
  | -- | Changes between versions could be reconciled
    Update a
  | -- | Changes between versions could not be reconciled
    Conflict
  deriving (Eq, Show, Functor)

instance Applicative Diff where
  pure = Same

  Update f <*> Update x = Update (f x)
  Update f <*> Same x = Update (f x)
  Same f <*> Update x = Update (f x)
  Same f <*> Same x = Same (f x)
  _ <*> _ = Conflict

class Diffable f where
  diff :: (a -> a -> Diff a) -> f a -> f a -> Diff (f a)

instance Apply Diffable fs => Diffable (Sum fs) where
  diff f x y = maybe Conflict id $ apply2' @Diffable (\inj a b -> inj <$> diff f a b) x y

instance Diffable f => Diffable (Free f) where
  diff f (Free x) (Free y) = Free <$> diff go x y
    where
      go a b = diff f a b
  diff f (Free x) _ = Update $ Free x
  diff f (Pure _) (Free y) = Update $ Free y
  diff f (Pure x) (Pure y) = Pure <$> f x y

class AST f a | a -> f where
  form :: f a -> a

instance AST f (Free f a) where
  form = Free

instance AST f (Fix f) where
  form = Fix

data PosInfo = Range (Row, Col) (Row, Col) | Point Row Col deriving (Show, Eq, Ord)

instance Semigroup PosInfo where
  (Range (aRow, aCol) (bRow, bCol)) <> (Range (cRow, cCol) (dRow, dCol)) = Range start end
    where
      start = case compare aRow cRow of
        GT -> (cRow, cCol)
        EQ -> (aRow, min aCol cCol)
        LT -> (aRow, aCol)
      end = case compare bRow dRow of
        GT -> (bRow, bCol)
        EQ -> (bRow, max bCol dCol)
        LT -> (dRow, dCol)
  (Range (xRow, xCol) (yRow, yCol)) <> (Point pRow pCol) =
    case compare xRow pRow of
      GT -> Range (pRow, pCol) (yRow, yCol)
      EQ -> Range (pRow, min xCol pCol) (yRow, yCol)
      LT -> case compare yRow pRow of
        GT -> Range (xRow, xCol) (yRow, yCol)
        EQ -> Range (xRow, xCol) (yRow, max yCol pCol)
        LT -> Range (xRow, xCol) (pRow, pCol)
  x@(Point _ _) <> y@(Range _ _) = y <> x
  x@(Point xRow xCol) <> y@(Point yRow yCol)
    | x == y = x
    | x < y = Range (xRow, xCol) (yRow, yCol)
    | otherwise = Range (yRow, yCol) (xRow, xCol)

instance Monoid PosInfo where
  mempty = Point (Row 0) (Col 0)

class HasPosInfo a where
  getPosInfo :: a -> PosInfo

instance HasPosInfo PosInfo where
  getPosInfo = id

start :: HasPosInfo e => Tagged t e a -> (Row, Col)
start (Tagged _ e) = case getPosInfo e of
  (Range x _) -> x
  (Point r c) -> (r, c)

end :: HasPosInfo e => Tagged t e a -> (Row, Col)
end (Tagged _ e) = case getPosInfo e of
  (Range _ y) -> y
  (Point r c) -> (r, c)

newtype Row = Row {unRow :: Natural} deriving (Num, Enum, Eq, Ord, Show)

newtype Col = Col {unCol :: Natural} deriving (Num, Enum, Eq, Ord, Show)

instance Monoid a => AST f (Cofree f a) where
  form = (:<) mempty

diffEq :: Eq a => a -> a -> Diff a
diffEq x y
  | x == y = Same x
  | otherwise = Conflict

diffToInfo :: Diff a -> Info a
diffToInfo (Update x) = Info x
diffToInfo (Same _) = NoInfo
diffToInfo _ = Contradiction

hasShape :: Diffable f => (a -> a -> Diff a) -> f a -> f a -> Bool
hasShape f x y
  | Conflict <- diff f x y = False
  | otherwise = True

data Tagged t e a = Tagged (t a) e
  deriving (Eq, Show, Functor, Foldable, Traversable)

deriveShow1 ''Tagged
deriveEq1 ''Tagged

untag :: Tagged t e a -> t a
untag (Tagged x _) = x

tagHisto ::
  (Recursive t) =>
  (Base t (t, a) -> a) ->
  t ->
  Cofree (Base t) a
tagHisto f x = result :< inner
  where
    subs = ((,) <*> tagHisto f) <$> Rec.project x
    inner = fmap snd subs
    result = f $ fmap (second $ \(x :< _) -> x) subs

uncofree :: Functor f => Cofree f a -> Free f b
uncofree (_ :< f) = Free $ uncofree <$> f
