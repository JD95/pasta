{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Runtime.Logic.Partial.List where

import Control.Applicative
import Control.Monad (join)
import Data.Bifunctor
import Data.Coerce
import Runtime.Logic.Partial
import Runtime.Prop
import Runtime.Ref

data ListF b a
  = Cons a b
  | Nil
  deriving (Eq)

instance EqH ListF where
  eqH Nil Nil = True
  eqH Nil (Cons _ _) = False
  eqH (Cons _ _) Nil = False
  eqH (Cons lx ly) (Cons rx ry) =
    lx == rx && ly == ry

instance Bifunctor ListF where
  bimap f g (Cons x y) = Cons (g x) (f y)
  bimap _ _ Nil = Nil

instance MergeableH ListF where
  mergeH (Old Nil) (New Nil) = None
  mergeH (Old Nil) (New (Cons _ _)) = Conflict
  mergeH (Old (Cons _ _)) (New Nil) = Conflict
  mergeH (Old (Cons oldX oldY)) (New (Cons newX newY)) =
    Cons <$> merge (Old oldX) (New newX) <*> merge (Old oldY) (New newY)

instance TopH ListF where
  isTopH (Cons a b) = isTop a && isTop b
  isTopH Nil = True

newtype List a = List {unList :: Partial ListF a}
  deriving newtype (Bottom, Top, Mergeable, Lattice, Eq)

instance Semigroup (List a) where
  (<>) = appendListL

instance Monoid (List a) where
  mempty = List (Partial $ Just Nil)

cons ::
  (Eq a, Lattice a, Eq (t (List a)), Eq (t a), GenTag m t, Alternative m, Ref m r) =>
  Cell t m r a ->
  Cell t m r (List a) ->
  Cell t m r (List a) ->
  m ()
cons x xs list = do
  prop [Watched x, Watched xs] list $ do
    hd <- readCell x
    tl <- readCell xs
    pure $ List $ Partial $ Just $ Cons hd (unList tl)
  prop [Watched list] x $ do
    unPartial . unList <$> readCell list >>= \case
      Just (Cons val _) -> pure val
      Just Nil -> empty
      Nothing -> pure bottom
  prop [Watched list] xs $ do
    unPartial . unList <$> readCell list >>= \case
      Just (Cons _ val) -> pure $ coerce val
      Just Nil -> empty
      Nothing -> pure $ List $ Partial $ Nothing

append ::
  (Eq a, Lattice a, Eq (t (List a)), GenTag m t, Alternative m, Ref m r) =>
  Cell t m r (List a) ->
  Cell t m r (List a) ->
  Cell t m r (List a) ->
  m ()
append xs ys zs = do
  prop [Watched xs, Watched ys] zs $ do
    left <- readCell xs
    right <- readCell ys
    pure $ left <> right
  prop [Watched xs, Watched zs] ys $ do
    front <- readCell xs
    full <- readCell zs
    pure $
      fromMaybeList $
        (drop . length)
          <$> (fromListL front)
          <*> (fromListL full)
  prop [Watched ys, Watched zs] xs $ do
    back <- readCell ys
    full <- readCell zs
    pure $
      fromMaybeList $
        (zipWith const . reverse)
          <$> (fromListL full)
          <*> (fromListL back)

fromMaybeList :: Maybe [a] -> List a
fromMaybeList = List . Partial . join . fmap (unPartial . unList . toListL)

fromListL :: List a -> Maybe [a]
fromListL (List (Partial Nothing)) = Nothing
fromListL (List (Partial (Just Nil))) = Just []
fromListL (List (Partial (Just (Cons x xs)))) = (x :) <$> fromListL (List xs)

toListL :: [a] -> List a
toListL [] = List (Partial (Just Nil))
toListL (x : xs) = List $ Partial (Just (Cons x (unList (toListL xs))))

appendListL :: List a -> List a -> List a
appendListL xs ys =
  List $ Partial $ join $ fmap (unPartial . unList . toListL) $ (<>) <$> (fromListL xs) <*> (fromListL ys)
