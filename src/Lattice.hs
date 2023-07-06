{-# LANGUAGE MagicHash #-}

module Lattice where

import Data.Coerce
import GHC.Prim

data Info a
  = Gain a
  | None
  | Conflict
  deriving (Show, Eq, Ord)

instance Functor Info where
  fmap f (Gain x) = Gain $ f x
  fmap _ None = None
  fmap _ Conflict = Conflict

instance Applicative Info where
  pure x = Gain x

  (Gain f) <*> (Gain x) = Gain $ f x
  (Gain _) <*> None = None
  (Gain _) <*> Conflict = Conflict
  None <*> (Gain _) = None
  None <*> None = None
  None <*> Conflict = None
  Conflict <*> (Gain _) = Conflict
  Conflict <*> None = Conflict
  Conflict <*> Conflict = Conflict

newtype New a = New a

newtype Old a = Old a

class Lattice a where
  bottom :: a
  isTop :: a -> Bool
  merge :: Old a -> New a -> Info a

newtype Peaceful a = Peaceful {unPeaceful :: a}

instance Lattice a => Lattice (Peaceful a) where
  bottom = Peaceful bottom
  isTop (Peaceful p) = isTop p
  merge old new =
    case merge (coerce old) (coerce new) of
      Gain g -> Gain $ Peaceful g
      None -> None
      Conflict -> None

instance Lattice () where
  bottom = ()
  isTop _ = True
  merge (Old ()) (New ()) = None

instance (Lattice a, Lattice b) => Lattice (a, b) where
  merge (Old (a, x)) (New (b, y)) =
    case (merge (Old a) (New b), merge (Old x) (New y)) of
      (Gain c, Gain z) -> Gain (c, z)
      (Gain c, None) -> Gain (c, x)
      (None, Gain z) -> Gain (a, z)
      (None, None) -> None
      _ -> Conflict

  bottom = (bottom, bottom)

  isTop (x, y) = isTop x && isTop y

instance (Eq a) => Lattice (Maybe a) where
  bottom = Nothing

  isTop (Just _) = True
  isTop Nothing = False

  merge (Old (Just x)) (New (Just y))
    | x == y = None
    | otherwise = Conflict
  merge (Old (Just _)) (New Nothing) = None
  merge (Old Nothing) (New (Just x)) = Gain (Just x)
  merge (Old Nothing) (New Nothing) = None

newtype Sum a b = Sum {unLatSum :: Maybe (Either a b)}
  deriving (Eq)

instance (Lattice a, Lattice b) => Lattice (Sum a b) where
  bottom = Sum Nothing

  isTop (Sum Nothing) = False
  isTop (Sum (Just (Left x))) = isTop x
  isTop (Sum (Just (Right x))) = isTop x

  merge (Old (Sum old)) (New (Sum new)) =
    case (old, new) of
      (_, Nothing) -> None
      (Nothing, Just x) -> Gain $ Sum $ Just x
      (Just (Left _), Just (Right _)) -> Conflict
      (Just (Right _), Just (Left _)) -> Conflict
      (Just (Left x), Just (Left y)) ->
        Sum . Just . Left <$> merge (Old x) (New y)
      (Just (Right x), Just (Right y)) ->
        Sum . Just . Right <$> merge (Old x) (New y)

newtype PtrOpt a = PtrOpt a

instance Lattice a => Lattice (PtrOpt a) where
  bottom = PtrOpt bottom

  isTop (PtrOpt x) = isTop x

  merge (Old (PtrOpt x)) (New (PtrOpt y)) =
    case reallyUnsafePtrEquality# x y of
      1# -> None
      _ -> PtrOpt <$> merge (Old x) (New y)

newtype LatOrd a = LatOrd a

instance Lattice a => Eq (LatOrd a) where
  LatOrd x == LatOrd y =
    case (merge (Old x) (New y), merge (Old y) (New x)) of
      (None, None) -> True
      _ -> False

instance Lattice a => Ord (LatOrd a) where
  LatOrd x <= LatOrd y =
    case merge (Old y) (New x) of
      None -> True
      _ -> False

  LatOrd x > LatOrd y =
    case merge (Old y) (New x) of
      None -> True
      _ -> False
