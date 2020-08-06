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

module Logic where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.ST
import Data.List hiding (product, sum)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.STRef
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import Numeric.Natural
import Prelude hiding (product, sum)

newtype CellRef s a = CellRef {unCellRef :: STRef s (Info a, [PropRef s])} deriving (Eq)

newtype PropRef s = PropRef {unPropRef :: STRef s (Seq (PropRef s) -> ST s (Seq (PropRef s)))} deriving (Eq)

----------------------------------------------------------------------------------------------------

class Merge a where
  merge :: a -> a -> Maybe a

data Info a = Info a | Contradiction | NoInfo deriving (Eq, Show)

instance Merge a => Semigroup (Info a) where
  (Info x) <> (Info y) = maybe Contradiction Info (merge x y)
  (Info x) <> NoInfo = Info x
  NoInfo <> (Info y) = Info y
  NoInfo <> NoInfo = NoInfo
  Contradiction <> _ = Contradiction
  _ <> Contradiction = Contradiction

instance Merge a => Monoid (Info a) where
  mempty = NoInfo

instance Merge Double where
  merge x y
    | x == y = Just x
    | otherwise = Nothing

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

popAlert :: Seq (PropRef s) -> Maybe (Seq (PropRef s), PropRef s)
popAlert alerts = do
  alert <- Seq.lookup 0 alerts
  pure (Seq.drop 1 alerts, alert)

pushAlert :: PropRef s -> Seq (PropRef s) -> Seq (PropRef s)
pushAlert alert alerts = alerts |> alert

content :: CellRef s a -> ST s (Info a)
content (CellRef ref) = do
  (info, _) <- readSTRef ref
  pure info

addContent :: (Eq a, Merge a) => Info a -> CellRef s a -> Seq (PropRef s) -> ST s (Seq (PropRef s))
addContent NoInfo _ alerts = pure alerts
addContent newInfo (CellRef ref) alerts = do
  (oldInfo, ns) <- readSTRef ref
  let mVal = do
        let result = newInfo <> oldInfo
        guard (result /= oldInfo)
        pure (result, ns)
  case mVal of
    Just (val, ns) -> do
      writeSTRef ref (val, ns)
      pure $ foldl' (|>) alerts ns
    Nothing -> pure alerts

newNeighbor :: PropRef s -> CellRef s a -> Seq (PropRef s) -> ST s (Seq (PropRef s))
newNeighbor n (CellRef ref) alerts = do
  (vals, ns) <- readSTRef ref
  if any ((/=) n) ns
    then do
      writeSTRef ref (vals, n : ns)
      pure (alerts |> n)
    else pure alerts

noInfoCheck :: [Info a] -> Info [a]
noInfoCheck = foldr (liftA2 (:)) (Info [])

propagator :: (Eq a, Merge a) => ([a] -> a) -> [CellRef s a] -> CellRef s a -> Seq (PropRef s) -> ST s (Seq (PropRef s))
propagator f ns target alerts = do
  todo <- fmap PropRef . newSTRef $ \as -> do
    inputs <- noInfoCheck <$> traverse content ns
    addContent (f <$> inputs) target as
  alerts' <- foldM (\alerts cell -> newNeighbor todo cell alerts) alerts ns
  pure $ alerts' |> todo

runAlerts :: Seq (PropRef s) -> ST s ()
runAlerts Empty = pure ()
runAlerts (PropRef next :<| rest) = do
  f <- readSTRef next
  rest' <- f rest
  runAlerts rest'

solve :: p s -> Eff '[State (Seq (PropRef s)), ST s] ()
solve (_ :: p s) = send . runAlerts =<< get @(Seq (PropRef s))

liftProp :: (Seq (PropRef s) -> ST s (Seq (PropRef s))) -> Eff '[State (Seq (PropRef s)), ST s] ()
liftProp f = do
  st <- get
  st' <- send $ f st
  put st'

runPropagator :: Eff '[State (Seq (PropRef s)), ST s] a -> a
runPropagator = runST . runM . evalState Empty

cell :: Eff '[State (Seq (PropRef s)), ST s] (CellRef s a)
cell = send $ CellRef <$> newSTRef (NoInfo, [])

constant :: (Eq a, Merge a) => a -> CellRef s a -> Eff '[State (Seq (PropRef s)), ST s] ()
constant val = liftProp . propagator (\_ -> val) []

adder :: (Eq a, Merge a, Num a) => CellRef s a -> CellRef s a -> CellRef s a -> Eff '[State (Seq (PropRef s)), ST s] ()
adder in1 in2 = liftProp . propagator (\[x, y] -> x + y) [in1, in2]

subtractor :: (Eq a, Merge a, Num a) => CellRef s a -> CellRef s a -> CellRef s a -> Eff '[State (Seq (PropRef s)), ST s] ()
subtractor in1 in2 = liftProp . propagator (\[x, y] -> x - y) [in1, in2]

multiplier :: (Eq a, Merge a, Num a) => CellRef s a -> CellRef s a -> CellRef s a -> Eff '[State (Seq (PropRef s)), ST s] ()
multiplier in1 in2 = liftProp . propagator (\[x, y] -> x * y) [in1, in2]

divider :: (Eq a, Merge a, Floating a) => CellRef s a -> CellRef s a -> CellRef s a -> Eff '[State (Seq (PropRef s)), ST s] ()
divider in1 in2 = liftProp . propagator (\[x, y] -> x / y) [in1, in2]

sum :: (Eq a, Merge a, Num a) => CellRef s a -> CellRef s a -> CellRef s a -> Eff '[State (Seq (PropRef s)), ST s] ()
sum in1 in2 out = do
  adder in1 in2 out
  subtractor out in1 in2
  subtractor out in2 in1

product :: (Eq a, Merge a, Floating a) => CellRef s a -> CellRef s a -> CellRef s a -> Eff '[State (Seq (PropRef s)), ST s] ()
product in1 in2 out = do
  multiplier in1 in2 out
  divider out in1 in2
  divider out in2 in1
