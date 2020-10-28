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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TypeCheck.Typed.Stages where

import AST.Core
import AST.Transform
import Control.Applicative
import Control.Monad
import Control.Monad.Free
import qualified Control.Monad.Free as Free
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.IO.Class
import Control.Monad.Primitive
import qualified Control.Monad.Trans.Free as F
import Data.Eq.Deriving
import Data.Foldable
import Data.Functor.Classes
import Data.Functor.Foldable (Fix (..), cata)
import Data.Hashable
import Data.Sum
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable
import Data.Vector (Vector)
import Display
import Logic
import Logic.Info
import Logic.Propagator
import Logic.Propagator.Class
import Logic.Propagator.PrimCell
import RIO hiding (Data)
import RIO.HashMap (HashMap)
import qualified RIO.HashMap as HashMap
import Text.Show.Deriving

data Ann a = Ann a a deriving (Eq, Show)

deriving instance Functor Ann

deriving instance Foldable Ann

deriving instance Traversable Ann

instance Diffable Ann where
  diff f (Ann _ _) (Ann _ _) = undefined

deriveEq1 ''Ann
deriveShow1 ''Ann

ann :: (Ann :< fs) => Free (Sum fs) a -> Free (Sum fs) a -> Free (Sum fs) a
ann val = Free . inject . Ann val

newtype Hole = Hole {unHole :: Int} deriving (Eq, Show)

instance DisplayF Ann where
  displayF (Ann x y) = x <> " : " <> y

hole :: Functor f => Int -> Free f Hole
hole i = Pure $ Hole i

newtype Given a = Given {unGiven :: a} deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''Given
deriveShow1 ''Given

newtype Expected a = Expected {unExpected :: a} deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''Expected
deriveShow1 ''Expected

data Err a
  = Errs [Err a]
  | Mismatch (Expected a) (Given a)
  | ConflictErr a a
  deriving (Eq, Functor, Foldable, Traversable)

deriveEq1 ''Err
deriveShow1 ''Err

instance Diffable Err where
  diff f x y = Update $ Errs [x, y]

instance DisplayF Err where
  displayF (Errs xs) = Text.unlines $ displayF <$> xs
  displayF (Mismatch (Expected expected) (Given actual)) =
    "Error! Mismatch between:\nExpected: "
      <> expected
      <> "\nActual: "
      <> actual
  displayF (ConflictErr x y) =
    "Error! Conflict between:\n> "
      <> x
      <> "\n> "
      <> y

errs :: (Err :< fs) => [Err (Free (Sum fs) a)] -> Free (Sum fs) a
errs = Free . inject . Errs

mismatch :: (Err :< fs) => Expected (Free (Sum fs) a) -> Given (Free (Sum fs) a) -> Free (Sum fs) a
mismatch x = Free . inject . Mismatch x

conflict :: (Err :< fs) => Free (Sum fs) a -> Free (Sum fs) a -> Free (Sum fs) a
conflict x = Free . inject . ConflictErr x

type Partial h = Free Typed h

-- | Expressions with Types and free variables
type Typed = Sum [Prim, Data, App, Lam, FreeVar, Ann, Err]
