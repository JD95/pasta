{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Runtime.Logic where

import Control.Applicative
import Control.Monad (join)
import Runtime.Prop
import Runtime.Ref

unify ::
  (Eq (t a), Lattice a, Alternative m, GenTag m t, Ref m r1, Ref m r2) =>
  Cell t m r1 a ->
  Cell t m r2 a ->
  m ()
unify x y = do
  prop [Watched x] y $ readCell x
  prop [Watched y] x $ readCell y

pair ::
  ( Eq (t a),
    Eq (t b),
    Eq (t (a, b)),
    Lattice a,
    Lattice b,
    Alternative m,
    GenTag m t,
    Ref m r1,
    Ref m r2,
    Ref m r3
  ) =>
  Cell t m r1 a ->
  Cell t m r2 b ->
  Cell t m r3 (a, b) ->
  m ()
pair x y z = do
  prop [Watched x, Watched y] z $ do
    (,) <$> readCell x <*> readCell y
  prop [Watched z] x $ do
    fst <$> readCell z
  prop [Watched z] y $ do
    snd <$> readCell z

newtype Product a = Product {unProduct :: Maybe [a]}
  deriving (Eq)
  deriving newtype (Lattice)

product ::
  (Eq a, Eq (t (Product a)), Alternative m, GenTag m t, Ref m r, Lattice a) =>
  [Cell t m r a] ->
  Cell t m r (Product a) ->
  m ()
product inputs output = do
  prop (Watched <$> inputs) output $ do
    Product . Just <$> traverse readCell inputs

data ListL a
  = Cons a (Maybe (ListL a))
  | Nil

newtype List a = List {unList :: Maybe (ListL a)}

instance Eq a => Eq (List a) where
  List Nothing == List Nothing = True
  List (Just Nil) == List (Just Nil) = True
  List (Just (Cons x xs)) == List (Just (Cons y ys)) =
    x == y && List xs == List ys
  List _ == List _ = False

instance (Eq a, Lattice a) => Lattice (List a) where
  bottom = List Nothing

  isTop (List (Just (Cons val (Just _)))) = isTop val
  isTop (List (Just Nil)) = True
  isTop _ = False

  merge (Old (List _)) (New (List Nothing)) =
    None
  merge (Old (List Nothing)) (New (List (Just x))) =
    Gain (List (Just x))
  merge (Old (List (Just old))) (New (List (Just new))) =
    case (old, new) of
      (Nil, Nil) -> None
      (Nil, Cons _ _) -> Conflict
      (Cons _ _, Nil) -> Conflict
      (Cons x xs, Cons y ys) ->
        if x == y
          then merge (Old (List xs)) (New (List ys))
          else Conflict

instance Semigroup (List a) where
  (<>) = appendListL

instance Monoid (List a) where
  mempty = List (Just Nil)

cons ::
  (Lattice a, Eq (t (List a)), Eq (t a), GenTag m t, Alternative m, Ref m r) =>
  Cell t m r a ->
  Cell t m r (List a) ->
  Cell t m r (List a) ->
  m ()
cons x xs list = do
  prop [Watched x, Watched xs] list $ do
    List . Just <$> (Cons <$> readCell x <*> (unList <$> readCell xs))
  prop [Watched list] x $ do
    readCell list >>= \case
      List (Just (Cons val _)) -> pure val
      List (Just Nil) -> empty
      List Nothing -> pure bottom
  prop [Watched list] xs $ do
    readCell list >>= \case
      List (Just (Cons _ val)) -> pure $ List val
      List (Just Nil) -> empty
      List Nothing -> pure $ List Nothing

append ::
  (Lattice a, Eq (t (List a)), GenTag m t, Alternative m, Ref m r) =>
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
fromMaybeList = List . join . fmap (unList . toListL)

fromListL :: List a -> Maybe [a]
fromListL (List Nothing) = Nothing
fromListL (List (Just Nil)) = Just []
fromListL (List (Just (Cons x xs))) = (x :) <$> fromListL (List xs)

toListL :: [a] -> List a
toListL [] = List (Just Nil)
toListL (x : xs) = List (Just (Cons x (unList (toListL xs))))

appendListL :: List a -> List a -> List a
appendListL xs ys =
  List $ join $ fmap (unList . toListL) $ (<>) <$> (fromListL xs) <*> (fromListL ys)

func ::
  (Alternative m, GenTag m t, Ref m r, Lattice a, Lattice b, Eq (t b)) =>
  (a -> b) ->
  Cell t m r a ->
  Cell t m r b ->
  m ()
func f input output = do
  prop [Watched input] output $ do
    x <- readCell input
    pure $ f x
