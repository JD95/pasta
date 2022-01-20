{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Runtime.Prop where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Runtime.Ref

data Info a
  = Gain a
  | None
  | Conflict

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
  Conflict <*> Conflict = Conflict

newtype New a = New a

newtype Old a = Old a

class Eq a => Lattice a where
  merge :: Old a -> New a -> Info a

  bottom :: a

  isTop :: a -> Bool

instance Eq a => Lattice (Maybe a) where
  merge (Old (Just x)) (New (Just y))
    | x == y = None
    | otherwise = Conflict
  merge (Old (Just x)) (New Nothing) = None
  merge (Old Nothing) (New (Just x)) = Gain (Just x)
  merge (Old Nothing) (New Nothing) = None

  bottom = Nothing

  isTop (Just _) = True
  isTop Nothing = False

instance (Lattice a, Lattice b) => Lattice (a, b) where
  merge (Old (a, x)) (New (b, y)) =
    (,)
      <$> merge (Old a) (New b)
      <*> merge (Old x) (New y)

  bottom = (bottom, bottom)

  isTop (x, y) = isTop x && isTop y

data Watched m r where
  Watched :: Lattice a => Cell m r a -> Watched m r

data Keep = Keep | Remove

data Prop m r = Prop
  { action :: r (m ())
  }

data Cell m r a = Cell
  { value :: r a,
    triggerWatchers :: r [m (a -> Bool)]
  }

instance Eq (r a) => Eq (Cell m r a) where
  x == y = value x == value y

cell :: forall a m r. Ref m r => a -> m (Cell m r a)
cell value = do
  x <- newRef value
  trig <- newRef []
  pure $ Cell x trig

inform :: (Lattice a, Alternative m, Ref m r) => Cell m r a -> a -> m ()
inform target new = do
  old <- readRef (value target)
  case merge (Old old) (New new) of
    Gain result -> do
      backTrackingWriteRef (value target) result
      trigs <- readRef (triggerWatchers target)
      fireAll result [] $ trigs
    None -> pure ()
    Conflict -> empty -- error "Kaboom! No backtracking yet"
  where
    fireAll _ kept [] = backTrackingWriteRef (triggerWatchers target) kept
    fireAll value kept (fire : xs) = do
      pred <- fire
      fireAll value xs $
        if pred value
          then fire : kept
          else kept

listenWhile :: (Alternative m, Ref m r) => (a -> Bool) -> Prop m r -> Cell m r a -> m ()
listenWhile cond prop c = do
  backTrackingModifyRef (triggerWatchers c) $ \others ->
    let new = do
          join $ readRef (action prop)
          pure cond
     in new : others

listenToOnce :: (Alternative m, Ref m r) => Prop m r -> Cell m r a -> m ()
listenToOnce = listenWhile (const False)

listenToAlways :: (Alternative m, Ref m r) => Prop m r -> Cell m r a -> m ()
listenToAlways = listenWhile (const True)

prop :: (Lattice a, Alternative m, Ref m r) => [Watched m r] -> Cell m r a -> m a -> m ()
prop [] target fire = inform target =<< fire
prop inputs@(Watched x : xs) target fire = do
  let fireInto = do
        result <- fire
        inform target result

      waitForInputs ref [] = do
        -- Now that all the inputs have usable
        -- values, fire the propagator now and
        -- every time one of them updates
        forM_ inputs $ \(Watched w) -> listenWhile (not . isTop) (Prop ref) w
        backTrackingWriteRef ref fireInto
        fireInto
      waitForInputs ref (Watched x : xs) = do
        value <- readRef $ value x
        if value == bottom
          then -- No usable input yet, wait until
          -- this cell gains input then check
          -- the rest
          do
            listenToOnce (Prop ref) x
            backTrackingWriteRef ref (waitForInputs ref xs)
          else -- This input has a usable value so
          -- go check the rest
            waitForInputs ref xs

  -- Create action ref with temp value
  propAction <- newRef (pure ())
  -- Override with proper action now that we have the ref
  writeRef propAction (waitForInputs propAction inputs)

  -- Have the first watched input trigger
  -- this propagator when it gains info
  listenToOnce (Prop propAction) x

  waitForInputs propAction inputs

liftP :: (Lattice a, Lattice b, Alternative m, Ref m r) => (a -> b) -> Cell m r a -> Cell m r b -> m ()
liftP f input output = do
  prop [Watched input] output $ do
    f <$> readRef (value input)

liftP2 :: (Lattice a, Lattice b, Lattice c, Alternative m, Ref m r) => (a -> b -> c) -> Cell m r a -> Cell m r b -> Cell m r c -> m ()
liftP2 f inA inB output = do
  prop [Watched inA, Watched inB] output $ do
    a <- readRef (value inA)
    b <- readRef (value inB)
    pure $ f a b

adder :: (Alternative m, Ref m r) => Cell m r (Maybe Int) -> Cell m r (Maybe Int) -> Cell m r (Maybe Int) -> m ()
adder x y z = do
  liftP2 (liftA2 (+)) x y z
  liftP2 (liftA2 (-)) z x y
  liftP2 (liftA2 (-)) z y x

pair ::
  (Lattice a, Lattice b, Alternative m, Ref m r) =>
  Cell m r a ->
  Cell m r b ->
  Cell m r (a, b) ->
  m ()
pair x y p = do
  liftP2 (,) x y p
  liftP fst p x
  liftP snd p y

tryList :: (Alternative m) => [a] -> m a
tryList = foldr (<|>) empty . fmap pure

var :: (Lattice a, Ref m r) => m (Cell m r a)
var = cell bottom
