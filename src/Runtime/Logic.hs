{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import Control.Monad.State
import Data.Foldable
import Data.Kind
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import GHC.Word
import Runtime.Prop
import Runtime.Ref

unify ::
  (Eq (t a), Lattice m a, Alternative m, GenTag m t, Ref m r1, Ref m r2) =>
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
    Lattice m a,
    Lattice m b,
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
pair x y pair = do
  prop [Watched x, Watched y] pair $ do
    (,) <$> readCell x <*> readCell y
  prop [Watched pair] x $ do
    fst <$> readCell pair
  prop [Watched pair] y $ do
    snd <$> readCell pair

data ListH h a
  = Cons (h a) (h (ListH h a))
  | Nil

newtype List a = List (Maybe (ListH Maybe a))

instance Eq a => Eq (List a) where
  List Nothing == List Nothing = True
  List (Just Nil) == List (Just Nil) = True
  List (Just (Cons x xs)) == List (Just (Cons y ys)) =
    x == y && List xs == List ys
  List _ == List _ = False

instance (Eq a, Monad m) => Lattice m (List a) where
  bottom = pure $ List Nothing

  isTop (List (Just (Cons (Just _) (Just _)))) = pure $ True
  isTop (List (Just Nil)) = pure $ True
  isTop _ = pure $ False

  merge (Old (List _)) (New (List Nothing)) =
    pure None
  merge (Old (List Nothing)) (New (List (Just x))) =
    pure $ Gain (List (Just x))
  merge (Old (List (Just x))) (New (List (Just y))) =
    case (x, y) of
      (Nil, Nil) -> pure None
      (Nil, Cons _ _) -> pure Conflict
      (Cons _ _, Nil) -> pure Conflict
      (Cons x xs, Cons y ys) ->
        if x == y
          then merge (Old (List xs)) (New (List ys))
          else pure Conflict
