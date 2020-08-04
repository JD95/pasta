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
import Data.Functor.Foldable (Fix (..))
import Data.Sum
import Data.Text (Text)
import Display
import Numeric.Natural (Natural)

(-:>) :: (Prim :< fs) => Fix (Sum fs) -> Fix (Sum fs) -> Fix (Sum fs)
i -:> o = Fix . inject $ Arr i o

new_ :: (Prim :< fs) => Text -> Fix (Sum fs) -> Fix (Sum fs)
new_ name = Fix . inject . NewTy name

ty :: (Prim :< fs) => Natural -> Fix (Sum fs)
ty = Fix . inject . Type

int :: (Prim :< fs) => Int -> Fix (Sum fs)
int = Fix . inject . PInt

-- | Lambda Terms
data Lam a = Lam Text a

deriving instance Functor Lam

instance Display Lam where
  displayF (Lam input body) = "\\" <> input <> " -> " <> body

lam :: (Lam :< fs) => Text -> Fix (Sum fs) -> Fix (Sum fs)
lam input = Fix . inject . Lam input

-- | Application terms
data App a = App a a

deriving instance Functor App

app :: (App :< fs) => Fix (Sum fs) -> Fix (Sum fs) -> Fix (Sum fs)
app func = Fix . inject . App func

instance Display App where
  displayF (App func input) = "(" <> func <> " " <> input <> ")"

newtype FreeVar a = FreeVar Text

deriving instance Functor FreeVar

free :: (FreeVar :< fs) => Text -> Fix (Sum fs)
free var = Fix . inject $ FreeVar var

instance Display FreeVar where
  displayF (FreeVar v) = v

type Core = Sum [Prim, Data, Lam, App, FreeVar]
