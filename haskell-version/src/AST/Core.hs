{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module AST.Core
  ( module AST.Core.Prim,
    module AST.Core.Data,
    App (..),
    Lam (..),
    FreeVar (..),
    Core,
    app,
    int,
    (-:>),
    new_,
    ty,
    lam,
    free,
  )
where

import AST.Core.Data
import AST.Core.Prim
import Control.Monad.Free
import Data.Functor.Classes
import Data.Functor.Foldable (Fix (..))
import Data.Sum
import Data.Text (Text)
import Display
import Numeric.Natural (Natural)

-- | Lambda Terms
data Lam a = Lam Text a deriving (Eq, Show)

deriving instance Functor Lam

deriving instance Foldable Lam

deriving instance Traversable Lam

instance Display Lam where
  displayF (Lam input body) = "\\" <> input <> " -> " <> body

lam :: (Lam :< fs) => Text -> Free (Sum fs) a -> Free (Sum fs) a
lam input = Free . inject . Lam input

instance Eq1 Lam where
  liftEq f (Lam a b) (Lam c d) = a == c && f b d

-- | Application terms
data App a = App a a deriving (Eq, Show)

deriving instance Functor App

deriving instance Foldable App

deriving instance Traversable App

app :: (App :< fs) => Free (Sum fs) a -> Free (Sum fs) a -> Free (Sum fs) a
app func = Free . inject . App func

instance Display App where
  displayF (App func input) = "(" <> func <> " " <> input <> ")"

newtype FreeVar a = FreeVar Text deriving (Eq, Show)

deriving instance Functor FreeVar

deriving instance Foldable FreeVar

deriving instance Traversable FreeVar

instance Eq1 App where
  liftEq f (App a b) (App c d) = f a c && f b d

free :: (FreeVar :< fs) => Text -> Free (Sum fs) a
free var = Free . inject $ FreeVar var

instance Display FreeVar where
  displayF (FreeVar v) = v

instance Eq1 FreeVar where
  liftEq _ (FreeVar x) (FreeVar y) = x == y

type Core = Sum [Prim, Data, Lam, App, FreeVar]
