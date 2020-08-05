{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.List hiding (product, sum)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Numeric.Natural
import Prelude hiding (product, sum)

newtype CellRef = CellRef {unCellRef :: Natural} deriving (Show, Eq)

newtype PropRef = PropRef {unPropRef :: Natural} deriving (Show, Eq)

data Allocator a = Allocator {counter :: Natural, values :: Map Natural a} deriving (Show)

alloc :: a -> Allocator a -> (Natural, Allocator a)
alloc val al =
  let i = counter al + 1
   in (i, al {counter = i, values = Map.insert i val (values al)})

type Prop a = '[State (PropST a)]

data PropST a = PropST
  { cells :: Allocator (Info a, [PropRef]),
    props :: Allocator (Eff (Prop a) ()),
    alerts :: Seq PropRef
  }

allocCell :: PropST a -> (CellRef, PropST a)
allocCell p =
  let (n, cs') = alloc (NoInfo, []) (cells p)
   in (CellRef n, p {cells = cs'})

addNeighbor :: PropRef -> CellRef -> PropST a -> Maybe (PropST a)
addNeighbor pr (CellRef cr) st = do
  (val, ns) <- Map.lookup cr (values $ cells st)
  if any ((==) pr) ns
    then Nothing
    else
      let cs' = Map.adjust (\_ -> (val, pr : ns)) cr (values $ cells st)
       in Just $ st {cells = (cells st) {values = cs'}}

allocProp :: (Eff (Prop a) ()) -> PropST a -> (PropRef, PropST a)
allocProp val p =
  let (n, ps') = alloc val (props p)
   in (PropRef n, p {props = ps'})

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

popAlert :: p a -> Eff (Prop a) (Maybe PropRef)
popAlert (_ :: p a) = do
  st <- get @(PropST a)
  case Seq.lookup 0 (alerts st) of
    Just alert -> do
      let alerts' = Seq.drop 1 (alerts st)
      put $ st {alerts = alerts'}
      pure (Just alert)
    Nothing -> pure Nothing

pushAlert :: p a -> PropRef -> Eff (Prop a) ()
pushAlert (_ :: p a) alert = do
  st <- get @(PropST a)
  put $ st {alerts = alerts st |> alert}

newCell :: p a -> Eff (Prop a) CellRef
newCell (_ :: p a) = do
  st <- get @(PropST a)
  let (ref, st') = allocCell st
  put $ st'
  pure ref

content :: CellRef -> Eff (Prop a) (Info a)
content (CellRef ref) = do
  st <- get
  case Map.lookup ref (values $ cells st) of
    Just (val, _) -> pure val
    Nothing -> error "Getting content of non-existant cell"

addContent :: (Eq a, Merge a) => Info a -> CellRef -> Eff (Prop a) ()
addContent NoInfo _ = pure ()
addContent newInfo (CellRef ref) = do
  PropST cs ps as <- get
  let mVal = do
        (oldInfo, ns) <- Map.lookup ref . values $ cs
        let result = newInfo <> oldInfo
        guard (result /= oldInfo)
        pure (result, ns)
  case mVal of
    Just (val, ns) -> do
      let cs' = cs {values = Map.adjust (first (const val)) ref (values cs)}
      let as' = foldl' (|>) as ns
      put $ (PropST cs' ps as')
    Nothing -> pure ()

newNeighbor :: p a -> PropRef -> CellRef -> Eff (Prop a) ()
newNeighbor (pxy :: p a) n cr = do
  st <- get @(PropST a)
  case addNeighbor n cr st of
    Just st' -> put st' *> pushAlert pxy n
    Nothing -> pure ()

noInfoCheck :: [Info a] -> Info [a]
noInfoCheck = foldr (liftA2 (:)) (Info [])

propagator :: (Show a, Eq a, Merge a) => ([a] -> a) -> [CellRef] -> CellRef -> Eff (Prop a) ()
propagator (f :: [a] -> a) ns target = do
  let prop = do
        inputs <- noInfoCheck <$> traverse content ns
        addContent (f <$> inputs) target
  st <- get
  let (todo, st') = allocProp prop st
  put $ st'
  forM_ ns (newNeighbor (Proxy @a) todo)
  pushAlert (Proxy @a) todo

constant :: (Show a, Eq a, Merge a) => a -> CellRef -> Eff (Prop a) ()
constant val = propagator (\_ -> val) []

runAlerts :: p a -> Eff (Prop a) ()
runAlerts (pxy :: p a) = do
  popAlert pxy >>= \case
    Just (PropRef alert) -> do
      st <- get @(PropST a)
      case Map.lookup alert (values $ props st) of
        Just prop -> prop *> runAlerts pxy
        Nothing -> runAlerts pxy
    Nothing -> pure ()

runPropagator :: Eff (Prop i) a -> a
runPropagator prop = run $ evalState initPropST prop
  where
    initPropST = PropST (Allocator 0 mempty) (Allocator 0 mempty) mempty

----------------------------------------------------------------------------------------------------

adder :: CellRef -> CellRef -> CellRef -> Eff (Prop Double) ()
adder in1 in2 = propagator (\[x, y] -> x + y) [in1, in2]

subtractor :: CellRef -> CellRef -> CellRef -> Eff (Prop Double) ()
subtractor in1 in2 = propagator (\[x, y] -> x - y) [in1, in2]

multiplier :: CellRef -> CellRef -> CellRef -> Eff (Prop Double) ()
multiplier in1 in2 = propagator (\[x, y] -> x * y) [in1, in2]

divider :: CellRef -> CellRef -> CellRef -> Eff (Prop Double) ()
divider in1 in2 = propagator (\[x, y] -> x / y) [in1, in2]

sum :: CellRef -> CellRef -> CellRef -> Eff (Prop Double) ()
sum in1 in2 out = do
  adder in1 in2 out
  subtractor out in1 in2
  subtractor out in2 in1

product :: CellRef -> CellRef -> CellRef -> Eff (Prop Double) ()
product in1 in2 out = do
  multiplier in1 in2 out
  divider out in1 in2
  divider out in2 in1
