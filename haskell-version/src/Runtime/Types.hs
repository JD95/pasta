{-# LANGUAGE DeriveFunctor #-}

module Runtime.Types where

import Control.Monad
import Data.Functor.Foldable (Fix (..), cata)
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

data RtCtl
  = -- | Check for path, branch
    RtBranch RtCtl (Vector (Match, RtCtl))
  | -- | Evaluate second with the side effects
    -- of evaluating the first
    RtEval RtCtl RtCtl
  | -- | Index into a product
    RtIndex RtCtl Natural
  | -- | Push args onto stack and eval func
    RtApp RtCtl (Vector RtCtl)
  | -- | Allocate an Empty cell
    RtAllocCell
      RtCtl
  | -- | Allocate a propagator
    RtAllocProp
      RtCtl
      -- ^ Target cell
      Natural
      -- ^ The index into the sources, to the right
      -- of which all cells are known to have info
      (Vector RtCtl)
      -- ^ The cells the propagator depends on
      RtCtl
      -- ^ The action to fire, the action
      -- the first input will the cell
      -- to inform with the value constructed
      -- with the rest of the inputs
  | -- | Inform cell using given value
    RtInformCell
      RtCtl
      -- ^ Cell to inform
      RtCtl
      -- ^ Value
  | -- | Add a propagator as a dependent of the cell
    RtAddCellDep
      RtCtl
      -- ^ Cell
      RtCtl
      -- ^ Propagator
  | -- | Allocate a new closure
    RtAllocThunk RtCtl (Vector RtCtl)
  | -- | Allocate a new product
    RtAllocProd (Vector RtCtl)
  | -- | Allocate a new tagged value
    RtAllocCon Natural RtCtl
  | -- | Allocate a primitive value
    RtAllocPrim PrimVal
  | -- | Reference to a variable on the stack frame
    RtStackVar Natural
  | -- | Reference to a free variable
    RtFreeVar Natural
  deriving (Show)

data RtClo r
  = -- | A propagator
    RtProp
      (r (RtClo r))
      -- ^ Target Cell
      (r Natural)
      -- ^ The index into the sources, to the right
      -- of which all cells are known to have info
      (Vector (r (RtClo r)))
      -- ^ The cells this propagator depends on
      RtCtl
      -- ^ The action fired when all cells have info
  | -- | Values filled by propagators
    RtCell
      (r (RtClo r))
      -- ^ The cell, holding the data
      RtCtl
      -- ^ How to merge information
      (Vector (r (RtClo r)))
      -- ^ The propagators to trigger if this cell updates
  | -- | An unevaluated thunk, result of application
    RtThunk
      RtCtl
      -- ^ Eval Code
      (Vector (r (RtClo r)))
      -- ^ Captured Free Variables
  | -- | An evaluated thunk
    RtWhnf (RtVal (r (RtClo r)))

data PrimVal
  = RtInt Int
  deriving (Show, Eq)

data RtVal a
  = RtPrim PrimVal
  | RtProd (Vector a)
  | RtCon Natural a
  deriving (Functor)

data RtEnv r = RtEnv {rtStackFrame :: Vector (r (RtClo r)), rtFrees :: Vector (r (RtClo r))}
