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

data Expr a where
  Lam :: a -> Expr a
  App :: a -> a -> Expr a
  Bound :: Natural -> Expr a

deriving instance Functor Expr

data Data a where
  List :: Vector a -> Data a
  Case :: a -> Map Natural a -> Data a
  In :: Natural -> a -> Data a
  Out :: Natural -> a -> Data a

deriving instance Functor Data 

data Type a where
  Ann :: a -> a -> Type a 
  Arr :: a -> a -> Type a
  Type :: Natural -> Type a

deriving instance Functor Type 

type TypedTerm = Expr :+: Data :+: Type

type Term = Expr :+: Data

type EvalTerm s = Stack s -> ST s (Fix Term)

data Stack s = Stack [STRef s (Thunk s)]

data Thunk s = Thunk (Either (EvalTerm s) (Fix Term)) (Stack s)

push a (Stack xs) = Stack (a:xs) 

index :: Natural -> Stack s -> Maybe (STRef s (Thunk s))
index _ (Stack []) = Nothing
index 0 (Stack (x:_)) = Just x
index n (Stack (_:rest)) = index (n - 1) (Stack rest)

forceThunk :: Thunk s -> ST s (Fix Term)
forceThunk (Thunk (Left f) st) = f st
forceThunk (Thunk (Right value) _) = pure value

eval :: Fix Term -> Fix Term 
eval term = runST $ (cata f term) (Stack []) where

  f :: Term (EvalTerm s) -> (EvalTerm s) 
  f = caseF goE goD  where

    goE :: Expr (EvalTerm s) -> EvalTerm s 
    goE (Lam body) st = body st 
    goE (App func input) st = do
      ref <- newSTRef (Thunk (Left input) st)
      func (push ref st)
    goE (Bound i) st = do
      case index i st of
        Just ref -> forceThunk =<< readSTRef ref
        Nothing -> error "Bound Symbol Error"

    goD :: Data (EvalTerm s) -> EvalTerm s
    goD (List xs) st = do
      xs' <- sequence $ xs <*> pure st
      pure . Fix . inj $ List xs'
    goD (Case x paths) st = do
      subject <- x st
      caseF
        (error "Case exception")
        (\case 
            In i value ->
              case Map.lookup i paths of
                Just path -> do
                  ref <- newSTRef (Thunk (Right value) st)
                  path (push ref st)
                Nothing -> error "Unhandled case exception"
            _ -> error "Case value error"
        )
        (unfix subject)
    goD (In i x) st = Fix . inj . In i <$> x st
    goD (Out i x) st = do
      subject <- x st
      caseF
        (error "Must index a list")
        (\case
            List xs -> pure (xs ! fromIntegral i)
            _ -> error "Must index a list")
        (unfix subject)
