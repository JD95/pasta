{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Runtime.Types where

import Control.Monad
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Debug.Trace
import Numeric.Natural
import Prelude hiding (const, id, log)

data Match
  = MInt Int
  | MCon Natural
  | MAny
  deriving (Show)

data PrimVal
  = RtInt Int
  deriving (Show, Eq)

data RtVal
  = RtPrim PrimVal
  | RtProd (Vector RtVal)
  | RtCon Natural RtVal
  | RtVar Natural
  | RtLam RtVal
  | RtApp RtVal RtVal
  deriving (Show, Eq)

makeBaseFunctor ''RtVal

unit :: RtVal
unit = RtProd Vec.empty