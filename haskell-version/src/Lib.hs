{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import           Control.Monad.ST (ST, runST)
import           Data.Comp.Sum
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Foldable (Fix(..), cata, unfix)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.STRef (STRef, newSTRef, readSTRef, modifySTRef)
import           Data.Text (Text)
import           Data.Vector (Vector, singleton, (!))
import           Numeric.Natural (Natural)

data Type a where
  Ann :: a -> a -> Type a 
  Arr :: a -> a -> Type a
  Type :: Natural -> Type a
deriving instance Functor Type 

-- | Lambda Terms
data Lam a = Lam Text a 
deriving instance Functor Lam

-- | Application terms
data App a = App a a
deriving instance Functor App

data Data a where
  -- | A grouping of values
  Struct :: Vector a -> Data a

  -- | Indexing into a struct
  Out :: Natural -> a -> Data a

  -- | A tag for union types
  In :: Natural -> a -> Data a

  -- | Branching on a value
  Case :: a -> Map Natural a -> Data a
deriving instance Functor Data

newtype FreeVar a = FreeVar Text 
deriving instance Functor FreeVar

-- ------------------------------------------------------------

-- | Expressions with Types and free variables 
type TypedTerm = Lam :+: App :+: Data :+: FreeVar :+: Type

-- -----------------------------------------------------------

-- | Strips types and converts FreeVar to Bound
mkRunnable :: Fix TypedTerm -> Fix (RunTerm s)
mkRunnable = _mkRunnable

-- | A Bound Lambda term.
--
-- Carries the ref to the bound value in the
-- body.
data BLam s a = BLam (STRef s (Closure s a)) a

-- | A polarized application.
--
-- Carries info about how to
-- evaluate input
data PApp a = PApp Pole a a
deriving instance Functor PApp

data Pole
  = Deep
  -- ^ Evaluate expression to normal form
  | Shallow
  -- ^ Evaluate expression to WHNF
  | Lazy
  -- ^ Suspend expression
  | Logic
  -- ^ Suspend with unresolved logic symbols 

-- | The runtime stack, holding pointers to closures
newtype Stack s a = Stack [STRef s (Closure s a)]

-- | A closure is either a suspended computation, a thunk,
-- or it's an expression in at least WHNF 
newtype Closure s a = Clo (Either (Stack s a -> ST s a, Stack s a) a) 

-- | All symbols at this point are bound within the stack
-- and have been converted to pointers
newtype Bound s a = Bound (STRef s (Closure s a)) 

-- | Expressions after type checking
-- ready to be evaluated
type RunTerm s = PApp :+: Data :+: BLam s :+: Bound s

-- -----------------------------------------------------------

eval :: Fix (RunTerm s) -> Fix (EvalTerm s)
eval = _eval

-- | Expressions during and after eval
type EvalTerm s = Data :+: Bound s :+: Closure s

-- forceClo :: Closure s a -> ST s a 
-- forceClo (Clo (Left f) st) = unThunk f st
-- forceClo (Clo (Right value) _) = pure value


-- push a (Stack xs) = Stack (a:xs) 
-- 
-- index :: Natural -> Stack s a -> Maybe (STRef s (Closure s a))
-- index _ (Stack []) = Nothing
-- index 0 (Stack (x:_)) = Just x
-- index n (Stack (_:rest)) = index (n - 1) (Stack rest)


-- eval :: Fix Expr -> Fix Expr 
-- eval term = runST $
--   caseF
--     (pure . Fix . inj . unfix) 
--     forceClo
--     (cata f term)
-- 
--   where
-- 
--   f :: Expr (EvalTerm s (Fix Expr)) -> EvalTerm s (Fix Expr)
--   f (Lam body) = body
--   f (List xs) = do
--     xs' <- sequence $ xs <*> pure st
--     pure . Fix . inj $ List xs'
--   f (In i x) = Fix . inj . In i <$> x st
-- 
--   -- App creates closures
--   f (App func input) = do
--     ref <- newSTRef (Clo (Left input) st)
--     func (push ref st)
--   f (Bound i) = do
--     case index i st of
--       Just ref -> forceClo =<< readSTRef ref
--       Nothing -> error "Bound Symbol Error"
-- 
--   -- Creates closures
--   f (Out i x) = do
--     subject <- x st
--     caseF
--       (error "Must index a list")
--       (\case
--           List xs -> pure (xs ! fromIntegral i)
--           _ -> error "Must index a list")
--       (unfix subject)
-- 
--   -- Evaluates closures
--   f (Case x paths) = do
--     subject <- x st
--     caseF
--       (error "Case exception")
--       (\case 
--           In i value ->
--             case Map.lookup i paths of
--               Just path -> do
--                 ref <- newSTRef (Clo (Right value) st)
--                 path (push ref st)
--               Nothing -> error "Unhandled case exception"
--           _ -> error "Case value error"
--      )
--      (unfix subject)


