{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Logic.Propagator where

import Control.Applicative
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Primitive
import Data.List hiding (product, sum)
import Data.Primitive.MutVar
import Data.Sequence (Seq (..), (|>))
import Prelude hiding (product, sum)

newtype CellRef m a = CellRef {unCellRef :: MutVar (PrimState m) (Info a, [PropRef m])}

newtype PropRef m = PropRef {unPropRef :: MutVar (PrimState m) (Seq (PropRef m) -> m (Seq (PropRef m)))}

class Merge a where
  merge :: a -> a -> Info a

data Info a
  = -- | Acts as multiplicative 0
    Contradiction
  | -- | Acts as multiplicative 1
    NoInfo
  | -- | Some info
    Info a
  deriving (Eq, Show)

instance Merge a => Semigroup (Info a) where
  (Info x) <> (Info y) = merge x y
  (Info x) <> NoInfo = Info x
  NoInfo <> (Info y) = Info y
  NoInfo <> NoInfo = NoInfo
  Contradiction <> _ = Contradiction
  _ <> Contradiction = Contradiction

instance Merge a => Monoid (Info a) where
  mempty = NoInfo

instance Merge Double where
  merge x y
    | x == y = NoInfo
    | otherwise = Contradiction

instance Functor Info where
  fmap f (Info x) = Info (f x)
  fmap _ NoInfo = NoInfo
  fmap _ Contradiction = Contradiction

instance Applicative Info where
  pure x = Info x

  (Info f) <*> (Info x) = Info (f x)
  NoInfo <*> _ = NoInfo
  _ <*> NoInfo = NoInfo
  Contradiction <*> _ = Contradiction
  _ <*> Contradiction = Contradiction

newtype Justified b a = Justified {unJustified :: (b, Info a)}

instance (Merge a, Ord b, Monoid b) => Merge (Justified b a) where
  merge (Justified (j1, x)) (Justified (j2, y)) =
    case x <> y of
      NoInfo ->
        if j1 == j2
          then NoInfo
          else Info $ Justified (min j1 j2, x)
      Contradiction ->
        if j1 == j2
          then Info $ Justified (j1, Contradiction)
          else Info $ Justified (min j1 j2, Contradiction)
      Info z -> Info $ Justified (j1 <> j2, Info z)

pushAlert :: PropRef m -> Seq (PropRef m) -> Seq (PropRef m)
pushAlert alert alerts = alerts |> alert

content :: (PrimMonad m) => CellRef m a -> m (Info a)
content (CellRef ref) = do
  (info, _) <- readMutVar ref
  pure info

addContent :: (PrimMonad m, Merge a) => Info a -> CellRef m a -> Seq (PropRef m) -> m (Seq (PropRef m))
addContent NoInfo _ alerts = pure alerts
addContent newInfo (CellRef ref) alerts = do
  (oldInfo, ns) <- readMutVar ref
  case newInfo <> oldInfo of
    NoInfo -> pure alerts
    val -> do
      writeMutVar ref (val, ns)
      pure $ foldl' (|>) alerts ns

newNeighbor :: (PrimMonad m) => PropRef m -> CellRef m a -> Seq (PropRef m) -> m (Seq (PropRef m))
newNeighbor n (CellRef ref) alerts = do
  (vals, ns) <- readMutVar ref
  if any ((==) (unPropRef n) . unPropRef) ns
    then pure alerts
    else do
      writeMutVar ref (vals, n : ns)
      pure (alerts |> n)

noInfoCheck :: [Info a] -> Info [a]
noInfoCheck = foldr (liftA2 (:)) (Info [])

propagator ::
  (PrimMonad m, Eq a, Merge a) =>
  ([a] -> a) ->
  [CellRef m a] ->
  CellRef m a ->
  Seq (PropRef m) ->
  m (Seq (PropRef m))
propagator f ns target alerts = do
  todo <- fmap PropRef . newMutVar $ \as -> do
    inputs <- noInfoCheck <$> traverse content ns
    addContent (f <$> inputs) target as
  alerts' <- foldM (\as c -> newNeighbor todo c as) alerts ns
  pure $ alerts' |> todo

runAlerts :: PrimMonad m => Seq (PropRef m) -> m ()
runAlerts Empty = pure ()
runAlerts (PropRef next :<| rest) = do
  f <- readMutVar next
  rest' <- f rest
  runAlerts rest'

runPropagator :: PrimBase m => (p m -> Eff '[State (Seq (PropRef m)), m] a) -> m a
runPropagator f = runM . evalState Empty $ (f undefined)

fill ::
  (PrimMonad m, Merge a, Members '[State (Seq (PropRef m)), m] es) =>
  Info a ->
  CellRef m a ->
  Eff es ()
fill info c = put =<< send . addContent info c =<< get

solve :: (PrimMonad m, Members '[State (Seq (PropRef m)), m] es) => env m -> Eff es ()
solve (_ :: env m) = send @m . runAlerts =<< get

liftProp ::
  (PrimMonad m, Members '[State (Seq (PropRef m)), m] es) =>
  (Seq (PropRef m) -> m (Seq (PropRef m))) ->
  Eff es ()
liftProp f = put =<< send . f =<< get

cell :: (PrimMonad m, Member m es) => p m -> Eff es (CellRef m a)
cell (_ :: p m) = send @m $ CellRef <$> newMutVar (NoInfo, [])

doubleCell :: (PrimMonad m, Member m es) => p m -> Eff es (CellRef m Double)
doubleCell = cell @_ @_ @_ @Double

constant ::
  (PrimMonad m, Eq a, Merge a) =>
  a ->
  CellRef m a ->
  Eff '[State (Seq (PropRef m)), m] ()
constant val = liftProp . propagator (\_ -> val) []
