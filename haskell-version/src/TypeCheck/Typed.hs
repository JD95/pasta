{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeCheck.Typed where

import AST.Core
import AST.Transform
import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Data.Functor.Classes
import Data.Functor.Foldable hiding (project)
import Data.Sum
import Data.Traversable
import Data.Vector (Vector)
import Logic.Info
import Logic.Propagator
import Logic.Propagator.PrimCell
import Unsafe.Coerce

data Ann a where
  Ann :: a -> a -> Ann a

deriving instance Functor Ann

deriving instance Foldable Ann

deriving instance Traversable Ann

instance Eq1 Ann where
  liftEq f (Ann a b) (Ann c d) = f a c && f b d

ann :: (Ann :< fs) => Fix (Sum fs) -> Fix (Sum fs) -> Fix (Sum fs)
ann val = Fix . inject . Ann val

newtype Hole m a = Hole {unHole :: PrimCell m (Typing m a)}

instance Eq1 (Hole m) where
  liftEq f (Hole x) (Hole y) = x == unsafeCoerce y

instance (Merge a) => Merge (Hole m a) where
  merge (OldInfo (Hole old)) (NewInfo (Hole new))
    | old == new = NoInfo
    | otherwise = Contradiction
  isTop _ = True

type Typing m = Sum [Prim, Data, App, Lam, FreeVar, Ann, Hole m]

-- | Expressions with Types and free variables
type TypedTerm = Sum [Prim, Data, App, Lam, FreeVar, Ann]

class (Network m, PrimMonad m) => UnifyF m f where
  unifyF :: f (Fix (Typing m)) -> f (Fix (Typing m)) -> m (f (Fix (Typing m)))

unify :: (Network m, PrimMonad m) => Fix (Typing m) -> Fix (Typing m) -> m (Fix (Typing m))
unify (x :: Fix (Typing m)) y = maybe empty id $ apply2' @(UnifyF m) (\inj a b -> Fix . inj <$> unifyF a b) (unfix x) (unfix y)

instance (Network m, PrimMonad m) => UnifyF m Prim where
  unifyF (PInt i) (PInt j) = guard (i == j) *> (pure $ PInt i)
  unifyF (Arr a b) (Arr c d) = Arr <$> unify a c <*> unify b d
  unifyF _ _ = error "UnifyF not fully implemented for Prim"

instance (Network m, PrimMonad m) => UnifyF m Data where
  unifyF (Struct xs) (Struct ys) = undefined
  unifyF _ _ = error "UnifyF not fully implemented for Data"

instance (Network m, PrimMonad m) => UnifyF m App where
  unifyF (App a b) (App c d) = undefined

instance (Network m, PrimMonad m) => UnifyF m Lam where
  unifyF (Lam name1 x) (Lam name2 y) = undefined

instance (Network m, PrimMonad m) => UnifyF m FreeVar where
  unifyF (FreeVar name1) (FreeVar name2) = undefined

instance (Network m, PrimMonad m) => UnifyF m Ann where
  unifyF (Ann val1 ann1) (Ann val2 ann2) = undefined

instance (Network m, PrimMonad m) => UnifyF m (Hole m) where
  unifyF x@(Hole (PrimCell xRef)) y@(Hole (PrimCell yRef)) = do
    (,) <$> content (unHole x) <*> content (unHole y) >>= \case
      (Info x', Info y') -> do
        -- Ensure that the available information
        -- in both holes unify
        _ <- unify (Fix x') (Fix y')
        -- Modify one hole to simply refer to the other
        -- This has the effect of setting x = y
        backtrackModify xRef (\(_, cs) -> (Info (inject y), cs))
        pure y
      (Info x', _) -> do
        -- Modify the second hole to simply refer to the first
        -- This has the effect of setting x = y
        backtrackModify yRef (\(_, cs) -> (Info (inject x), cs))
        pure x
      (NoInfo, NoInfo) -> do
        -- Modify the first hole to simply refer to the second
        -- This has the effect of setting x = y
        backtrackModify xRef (\(_, cs) -> (Info (inject y), cs))
        pure y
      _ -> empty

class (Network m, PrimMonad m) => Zonk m f where
  zonk :: p m -> f (Fix TypedTerm) -> m (Fix TypedTerm)

-- | Used to pass non-hole parts of AST through zonking process
zonkPass ::
  (TypedTerm ~ Sum fs, Network m, PrimMonad m, Traversable f, f :< fs) =>
  p m ->
  f (Fix TypedTerm) ->
  m (Fix TypedTerm)
zonkPass r = fmap (Fix . inject) . sequence . fmap (zonk r . unfix)

applyZonk :: Fix (Typing m) -> m (Fix TypedTerm)
applyZonk = 

instance (Network m, PrimMonad m, Apply (Zonk m) fs) => Zonk m (Sum fs) where
  zonk (r :: p m) x = apply @(Zonk m) (zonk r) x

instance (Network m, PrimMonad m) => Zonk m (Hole m) where
  zonk r (Hole x) = do
    content x >>= \case
      Info x' -> zonk r x'
      _ -> empty

instance (Network m, PrimMonad m) => Zonk m Prim where
  zonk = zonkPass

instance (Network m, PrimMonad m) => Zonk m Data where
  zonk = zonkPass

instance (Network m, PrimMonad m) => Zonk m Lam where
  zonk = zonkPass

instance (Network m, PrimMonad m) => Zonk m App where
  zonk = zonkPass

instance (Network m, PrimMonad m) => Zonk m FreeVar where
  zonk = zonkPass

instance (Network m, PrimMonad m) => Zonk m Ann where
  zonk = zonkPass

class TypeCheck f where
  checkF :: (Network m, PrimMonad m) => f (Check m) -> Check m

newtype Check m = Check {unCheck :: Vector (Fix (Typing m)) -> m (Fix (Typing m))}

instance Apply TypeCheck fs => TypeCheck (Sum fs) where
  checkF = apply @TypeCheck checkF

instance TypeCheck Data where
  checkF (Struct xs) = Check $ \env -> undefined

-- Unit being represented by
-- an empty product
-- result <- newPrimCell
-- sub <- struct <$> traverse (\(Check i) -> Fix . inject <$> i env) xs
-- inform (Info sub) result
-- _ result
