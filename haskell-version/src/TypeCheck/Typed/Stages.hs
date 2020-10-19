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
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sum
import Data.Text (Text, pack)
import Data.Traversable
import Data.Vector (Vector)
import Display
import Logic
import Logic.Info
import Logic.Propagator
import Logic.Propagator.Class
import Logic.Propagator.PrimCell
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

instance Display Ann where
  displayF (Ann x y) = x <> " : " <> y

hole :: Functor f => Int -> Free f Hole
hole i = Pure $ Hole i

type Partial h = Free Typed h

-- | Expressions with Types and free variables
type Typed = Sum [Prim, Data, App, Lam, FreeVar, Ann]
