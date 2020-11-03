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

class InjAnn f where
  injAnn :: Ann a -> f a

instance Ann :< f => InjAnn (Sum f) where
  injAnn = inject

ann :: (AST f a, InjAnn f) => a -> a -> a
ann val = form . injAnn . Ann val

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
  | Mismatch PosInfo (Expected a) (Given a)
  | ConflictErr PosInfo a a
  deriving (Eq, Functor, Foldable, Traversable)

deriveEq1 ''Err
deriveShow1 ''Err

instance Diffable Err where
  diff f x y = Update $ Errs [x, y]

instance DisplayF Err where
  displayF (Errs xs) = Text.unlines $ displayF <$> xs
  displayF (Mismatch pos (Expected expected) (Given actual)) =
    "Error (" <> Text.pack (show pos) <> ") Mismatch between:\n> "
      <> expected
      <> "\nActual: "
      <> actual
  displayF (ConflictErr pos x y) =
    "Error (" <> Text.pack (show pos) <> ") Conflict between:\n> "
      <> x
      <> "\n> "
      <> y

class InjErr f where
  injErr :: Err a -> f a

instance Err :< fs => InjErr (Sum fs) where
  injErr = inject

errs :: (AST f a, InjErr f) => [Err a] -> a
errs = form . injErr . Errs

mismatch :: (AST f a, InjErr f) => PosInfo -> Expected a -> Given a -> a
mismatch p x = form . injErr . Mismatch p x

conflict :: (AST f a, InjErr f) => PosInfo -> a -> a -> a
conflict p x = form . injErr . ConflictErr p x

type Partial h = Free Typed h

-- | Expressions with Types and free variables
type Typed = Sum [Prim, Data, App, Lam, FreeVar, Ann, Err]
