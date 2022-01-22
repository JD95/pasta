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

class (Monad m, Eq a) => Lattice m a where
  bottom :: m a
  isTop :: a -> m Bool
  merge :: Old a -> New a -> m (Info a)

instance (Monad m, Eq a) => Lattice m (Maybe a) where
  bottom = pure Nothing

  isTop (Just _) = pure True
  isTop Nothing = pure False

  merge (Old (Just x)) (New (Just y))
    | x == y = pure None
    | otherwise = pure Conflict
  merge (Old (Just x)) (New Nothing) = pure None
  merge (Old Nothing) (New (Just x)) = pure $ Gain (Just x)
  merge (Old Nothing) (New Nothing) = pure None

instance (Monad m, Lattice m a, Lattice m b) => Lattice m (a, b) where
  merge (Old (a, x)) (New (b, y)) = do
    left <- merge (Old a) (New b)
    right <- merge (Old x) (New y)
    pure $ (,) <$> left <*> right

  bottom = (,) <$> bottom <*> bottom

  isTop (x, y) = (&&) <$> isTop x <*> isTop y

data Watched m r where
  Watched :: Lattice m a => Cell m r a -> Watched m r

data Keep = Keep | Remove

data Prop m r = Prop
  { action :: r (m ())
  }

data Cell m r a = Cell
  { value :: r a,
    triggerWatchers :: r [m (a -> m Bool)]
  }

instance Eq (r a) => Eq (Cell m r a) where
  x == y = value x == value y

cell :: forall a m r. Ref m r => a -> m (Cell m r a)
cell value = do
  x <- newRef value
  trig <- newRef []
  pure $ Cell x trig

inform :: (Lattice m a, Alternative m, Ref m r) => Cell m r a -> a -> m ()
inform target new = do
  old <- readRef (value target)
  merge (Old old) (New new) >>= \case
    Gain result -> do
      backTrackingWriteRef (value target) result
      trigs <- readRef (triggerWatchers target)
      fireAll result [] $ trigs
    None -> pure ()
    Conflict -> empty
  where
    fireAll _ kept [] = backTrackingWriteRef (triggerWatchers target) kept
    fireAll value kept (fire : xs) = do
      pred <- fire
      next <-
        pred value >>= \case
          True -> pure $ fire : kept
          False -> pure $ kept
      fireAll value xs next

listenWhile :: (Alternative m, Ref m r) => (a -> m Bool) -> Prop m r -> Cell m r a -> m ()
listenWhile cond prop c = do
  backTrackingModifyRef (triggerWatchers c) $ \others ->
    let new = do
          join $ readRef (action prop)
          pure cond
     in new : others

listenToOnce :: (Alternative m, Ref m r) => Prop m r -> Cell m r a -> m ()
listenToOnce = listenWhile (const $ pure False)

listenToAlways :: (Alternative m, Ref m r) => Prop m r -> Cell m r a -> m ()
listenToAlways = listenWhile (const $ pure True)

prop :: (Lattice m a, Alternative m, Ref m r) => [Watched m r] -> Cell m r a -> m a -> m ()
prop [] target fire = inform target =<< fire
prop inputs@(Watched x : xs) target fire = do
  let fireInto = do
        result <- fire
        inform target result

      waitForInputs ref [] = do
        -- Now that all the inputs have usable
        -- values, fire the propagator now and
        -- every time one of them updates
        forM_ inputs $ \(Watched w) -> listenWhile (fmap not . isTop) (Prop ref) w
        backTrackingWriteRef ref fireInto
        fireInto
      waitForInputs ref (Watched x : xs) = do
        value <- readRef $ value x
        bot <- bottom
        if value == bot
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

liftP :: (Lattice m a, Lattice m b, Alternative m, Ref m r) => (a -> b) -> Cell m r a -> Cell m r b -> m ()
liftP f input output = do
  prop [Watched input] output $ do
    f <$> readRef (value input)

liftP2 :: (Lattice m a, Lattice m b, Lattice m c, Alternative m, Ref m r) => (a -> b -> c) -> Cell m r a -> Cell m r b -> Cell m r c -> m ()
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
  (Lattice m a, Lattice m b, Alternative m, Ref m r) =>
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

var :: (Lattice m a, Ref m r) => m (Cell m r a)
var = cell =<< bottom
