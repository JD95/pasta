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
import Runtime.Lattice
import Runtime.Logic.Context
import Runtime.Logic.Partial
import Runtime.Logic.Partial.List
-- import Runtime.Prop
import Runtime.Ref

{-
unify ::
  (Eq a, Eq (t a), Lattice a, Alternative m, GenTag m t, Ref m r1, Ref m r2) =>
  Cell t m r1 a ->
  Cell t m r2 a ->
  m ()
unify x y = do
  prop [Watched x] y $ readCell x
  prop [Watched y] x $ readCell y

pair ::
  ( Eq a,
    Eq b,
    Eq (t a),
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
  deriving newtype (Mergeable, Top, Bottom, Lattice)

product ::
  (Eq a, Eq (t (Product a)), Alternative m, GenTag m t, Ref m r, Lattice a) =>
  [Cell t m r a] ->
  Cell t m r (Product a) ->
  m ()
product inputs output = do
  prop (Watched <$> inputs) output $ do
    Product . Just <$> traverse readCell inputs

func ::
  (Eq a, Alternative m, GenTag m t, Ref m r, Lattice a, Lattice b, Eq (t b)) =>
  (a -> b) ->
  Cell t m r a ->
  Cell t m r b ->
  m ()
func f input output = do
  prop [Watched input] output $ do
    x <- readCell input
    pure $ f x
-}
