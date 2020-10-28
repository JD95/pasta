{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AST.Core
  ( module AST.Core.Prim,
    module AST.Core.Data,
    App (..),
    Lam (..),
    FreeVar (..),
    Core,
    app,
    lam,
    free,
  )
where

import AST.Core.Data
import AST.Core.Prim
import AST.Transform
import Control.Monad.Free
import Data.Eq.Deriving
import Data.Functor.Classes
import Data.Functor.Foldable (Fix (..))
import Data.Sum
import Data.Text (Text)
import Display
import Numeric.Natural (Natural)
import RIO hiding (Data)
import Text.Show.Deriving

-- | Lambda Terms
data Lam a = Lam Text a deriving (Eq, Show)

deriving instance Functor Lam

deriving instance Foldable Lam

deriving instance Traversable Lam

instance DisplayF Lam where
  displayF (Lam input body) = "\\" <> input <> " -> " <> body

lam :: (Lam :< fs) => Text -> Free (Sum fs) a -> Free (Sum fs) a
lam input = Free . inject . Lam input

deriveShow1 ''Lam
deriveEq1 ''Lam

instance Diffable Lam where
  diff f (Lam _ _) (Lam _ _) = undefined

-- | Application terms
data App a = App a a deriving (Eq, Show)

deriving instance Functor App

deriving instance Foldable App

deriving instance Traversable App

deriveShow1 ''App
deriveEq1 ''App

instance Diffable App where
  diff f (App _ _) (App _ _) = undefined

app :: (App :< fs) => Free (Sum fs) a -> Free (Sum fs) a -> Free (Sum fs) a
app func = Free . inject . App func

instance DisplayF App where
  displayF (App func input) = "(" <> func <> " " <> input <> ")"

newtype FreeVar a = FreeVar Text deriving (Eq, Show)

instance Diffable FreeVar where
  diff f (FreeVar x) (FreeVar y) = FreeVar <$> x `diffEq` y

deriving instance Functor FreeVar

deriving instance Foldable FreeVar

deriving instance Traversable FreeVar

instance DisplayF FreeVar where
  displayF (FreeVar v) = v

deriveShow1 ''FreeVar
deriveEq1 ''FreeVar

free :: (FreeVar :< fs) => Text -> Free (Sum fs) a
free var = Free . inject $ FreeVar var

type Core = Sum [Prim, Data, Lam, App, FreeVar]
