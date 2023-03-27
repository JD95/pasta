{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
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
import Prelude hiding (Bool (..))

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
