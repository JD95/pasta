{-# LANGUAGE TypeFamilies #-}

module Perle.Lattice.Diet
  ( Diet
  , singleton
  , intersection
  , delete
  , takeLT
  , takeGT
  , isPoint
  , fromIntervals
  , intervals
  , enumerate
  , member
  , narrowerThan
  , distanceCovered
  )where

import Data.Diet.Set.Lifted (Set)
import qualified Data.Diet.Set.Lifted as Set

import Perle.Lattice
import qualified Perle.Lattice as Lat
import Perle.Lattice.Logical

newtype Diet a = Diet { unDiet :: Set a }
  deriving (Show, Eq)

instance (Num a, Bounded a, Enum a, Ord a) => Lattice (Diet a) where
  bottom = singleton minBound maxBound

  isTop = isPoint

  merge (Old old) (New new)
    | null (intervals result) = Conflict
    | result `narrowerThan` old = Gain result
    | otherwise = None

    where
      result = intersection old new

instance (Ord a, Enum a, Bounded a, Num a) => Logical (Diet a) where
  type Realized (Diet a) = a
  assert x = singleton x x
  realize = enumerate

singleton :: Ord a => a -> a -> Diet a
singleton low high = Diet $ Set.singleton low high

intersection :: (Ord a, Enum a) => Diet a -> Diet a -> Diet a
intersection (Diet x) (Diet y) = Diet (Set.intersection x y)

delete :: (Ord a, Enum a) => a -> Diet a -> Diet a
delete x (Diet xs) = Diet (Set.difference xs (Set.singleton x x))

takeLT :: Ord a => a -> Diet a -> Diet a
takeLT x (Diet xs) = Diet (Set.belowInclusive x xs)

takeGT :: Ord a => a -> Diet a -> Diet a
takeGT x (Diet xs) = Diet (Set.aboveInclusive x xs)

intervals :: Diet a -> [(a,a)]
intervals = Set.foldr (\x y -> ((x, y):)) [] . unDiet

fromIntervals :: (Enum a, Ord a) => [(a,a)] -> Diet a
fromIntervals = Diet . Set.fromList

-- | Is the distance of the combined intervals for
-- the first smaller than the same for the second
narrowerThan :: (Ord a, Num a) => Diet a -> Diet a -> Bool
narrowerThan xs ys = distanceCovered xs < distanceCovered ys

distanceCovered :: (Num a) => Diet a -> a
distanceCovered = sum . fmap measure . intervals where
  measure = (1 +) . abs . uncurry (flip (-))

isPoint :: Eq a => Diet a -> Bool
isPoint xs =
  case intervals xs of
    [] -> False
    [(x,y)] -> x == y
    _:_:_ -> False

enumerate :: Enum a => Diet a -> [a]
enumerate (Diet xs) = Set.foldr (\x y -> (enumFromTo x y <>)) [] xs

member :: Ord a => a -> Diet a -> Bool
member x (Diet xs) = Set.member x xs
