{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Data.Word
import Debug.Trace
import Prelude hiding (const, id, log)

data Match
  = MInt Int
  | MCon Word32
  | MAny
  deriving (Show)

data PrimVal
  = RtInt Int
  deriving (Show, Eq)

data RtVal
  = RtPrim PrimVal
  | RtProd (Vector RtVal)
  | RtCon Word32 RtVal
  | RtVar Word32
  | RtLam RtVal
  | RtArr RtVal RtVal
  | RtApp RtVal RtVal
  | RtTy
  deriving (Show, Eq)

makeBaseFunctor ''RtVal

deriving instance Eq a => Eq (RtValF a)

unit :: RtVal
unit = RtProd Vec.empty

unitF :: RtValF a
unitF = RtProdF Vec.empty
