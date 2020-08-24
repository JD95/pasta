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
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.State
import Data.IORef
import Data.List hiding (product, sum)
import Data.Sequence (Seq (..), (|>))
import Logic.Info
import Logic.Propagator
import Prelude hiding (product, sum)

newtype CellRef a = CellRef {unCellRef :: IORef (Info a, [PropRef])}

newtype PropRef = PropRef {unPropRef :: IORef (Seq (PropRef) -> IO (Seq (PropRef)))}

----------------------------------------------------------------------------------------------------

pushAlert :: PropRef -> Seq PropRef -> Seq PropRef
pushAlert alert alerts = alerts |> alert

content :: CellRef a -> IO (Info a)
content (CellRef ref) = do
  (info, _) <- readIORef ref
  pure info

fill ::
  (Eq a, Merge a) => Info a -> CellRef a -> Eff '[State (Seq PropRef), IO] ()
fill info c = put =<< send . addContent info c =<< get

addContent :: (Eq a, Merge a) => Info a -> CellRef a -> Seq PropRef -> IO (Seq PropRef)
addContent NoInfo _ alerts = pure alerts
addContent newInfo (CellRef ref) alerts = do
  (oldInfo, ns) <- readIORef ref
  let mVal = do
        let result = newInfo <> oldInfo
        guard (result /= oldInfo)
        pure result
  case mVal of
    Just val -> do
      writeIORef ref (val, ns)
      pure $ foldl' (|>) alerts ns
    Nothing -> pure alerts

newNeighbor :: PropRef -> CellRef a -> Seq PropRef -> IO (Seq PropRef)
newNeighbor n (CellRef ref) alerts = do
  (vals, ns) <- readIORef ref
  if any ((==) (unPropRef n) . unPropRef) ns
    then pure alerts
    else do
      writeIORef ref (vals, n : ns)
      pure (alerts |> n)

noInfoCheck :: [Info a] -> Info [a]
noInfoCheck = foldr (liftA2 (:)) (Info [])

data CellStatus = CallStatus {hasInfo :: IO Bool, addNeighbor :: PropRef -> Seq PropRef -> IO (Seq PropRef)}

mkCellStatus :: CellRef a -> CellStatus
mkCellStatus ref =
  CallStatus
    { hasInfo = do
        content ref >>= \case
          NoInfo -> pure False
          _ -> pure True,
      addNeighbor = \p -> newNeighbor p ref
    }

propagator ::
  (Eq a, Merge a) =>
  IO (Info a) ->
  [CellStatus] ->
  CellRef a ->
  Seq PropRef ->
  IO (Seq PropRef)
propagator action ns target alerts = do
  todo <- fmap PropRef . newIORef $ \as -> do
    allHaveInfo <- fmap and . traverse hasInfo $ ns
    if allHaveInfo
      then do
        result <- action
        addContent result target as
      else pure as
  alerts' <- foldM (\as c -> addNeighbor c todo as) alerts ns
  pure $ alerts' |> todo

runAlerts :: Seq PropRef -> IO ()
runAlerts Empty = pure ()
runAlerts (PropRef next :<| rest) = do
  f <- readIORef next
  rest' <- f rest
  runAlerts rest'

solve :: Eff '[State (Seq PropRef), IO] ()
solve = send . runAlerts =<< get

liftProp :: (Seq PropRef -> IO (Seq PropRef)) -> Eff '[State (Seq PropRef), IO] ()
liftProp f = put =<< send . f =<< get

runPropagator :: Eff '[State (Seq PropRef), IO] a -> IO a
runPropagator = runM . evalState Empty

cell :: Eff '[State (Seq PropRef), IO] (CellRef a)
cell = send $ CellRef <$> newIORef (NoInfo, [])

constant ::
  (Eq a, Merge a) =>
  a ->
  CellRef a ->
  Eff '[State (Seq PropRef), IO] ()
constant val = liftProp . propagator (pure $ Info val) []

binProp ::
  (Eq c, Merge c) =>
  (a -> b -> c) ->
  CellRef a ->
  CellRef b ->
  CellRef c ->
  Eff '[State (Seq PropRef), IO] ()
binProp f in1 in2 out = do
  let action = do
        Info x <- content in1
        Info y <- content in2
        pure . Info $ f x y
      stats = [mkCellStatus in1, mkCellStatus in2]
   in liftProp $ propagator action stats out

adder ::
  (Eq a, Merge a, Num a) =>
  CellRef a ->
  CellRef a ->
  CellRef a ->
  Eff '[State (Seq PropRef), IO] ()
adder = binProp (+)

subtractor ::
  (Eq a, Merge a, Num a) =>
  CellRef a ->
  CellRef a ->
  CellRef a ->
  Eff '[State (Seq PropRef), IO] ()
subtractor = binProp (-)

multiplier ::
  (Eq a, Merge a, Num a) =>
  CellRef a ->
  CellRef a ->
  CellRef a ->
  Eff '[State (Seq PropRef), IO] ()
multiplier = binProp (*)

divider ::
  (Eq a, Merge a, Floating a) =>
  CellRef a ->
  CellRef a ->
  CellRef a ->
  Eff '[State (Seq PropRef), IO] ()
divider = binProp (/)

sum ::
  (Eq a, Merge a, Num a) =>
  CellRef a ->
  CellRef a ->
  CellRef a ->
  Eff '[State (Seq PropRef), IO] ()
sum in1 in2 out = do
  adder in1 in2 out
  subtractor out in1 in2
  subtractor out in2 in1

product ::
  (Eq a, Merge a, Floating a) =>
  CellRef a ->
  CellRef a ->
  CellRef a ->
  Eff '[State (Seq PropRef), IO] ()
product in1 in2 out = do
  multiplier in1 in2 out
  divider out in1 in2
  divider out in2 in1
