{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Runtime.Prop where

import Control.Applicative ( Alternative(empty) )
import Control.Monad ( join, forM_ )
import Runtime.Ref
    ( backTrackingWriteRef, backTrackingModifyRef, MonadRef(..) )

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
  Conflict <*> (Gain _) = Conflict
  Conflict <*> None = Conflict
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
  merge (Old (Just _)) (New Nothing) = pure None
  merge (Old Nothing) (New (Just x)) = pure $ Gain (Just x)
  merge (Old Nothing) (New Nothing) = pure None

instance (Monad m) => Lattice m () where
  bottom = pure ()
  isTop _ = pure True
  merge (Old ()) (New ()) = pure None

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

newtype Prop m r = Prop
  { action :: r (m ())
  }

data Cell m r a = Cell
  { value :: r a,
    triggerWatchers :: r [m (a -> m Bool)]
  }

instance Eq (r a) => Eq (Cell m r a) where
  x == y = value x == value y

cell :: forall a m. MonadRef m => a -> m (Cell m (Ref m) a)
cell val = do
  x <- newRef val
  trig <- newRef []
  pure $ Cell x trig

inform :: (Lattice m a, Alternative m, MonadRef m) => Cell m (Ref m) a -> a -> m ()
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
    fireAll x kept (fire : xs) = do
      p <- fire
      next <-
        p x >>= \case
          True -> pure $ fire : kept
          False -> pure $ kept
      fireAll x xs next

listenWhile :: (Alternative m, MonadRef m) => (a -> m Bool) -> Prop m (Ref m) -> Cell m (Ref m) a -> m ()
listenWhile cond p c = do
  backTrackingModifyRef (triggerWatchers c) $ \others ->
    let new = do
          join $ readRef (action p)
          pure cond
     in new : others

listenToOnce :: (Alternative m, MonadRef m) => Prop m (Ref m) -> Cell m (Ref m) a -> m ()
listenToOnce = listenWhile (const $ pure False)

listenToAlways :: (Alternative m, MonadRef m) => Prop m (Ref m) -> Cell m (Ref m) a -> m ()
listenToAlways = listenWhile (const $ pure True)

prop :: (Lattice m a, Alternative m, MonadRef m) => [Watched m (Ref m)] -> Cell m (Ref m) a -> m a -> m ()
prop [] target fire = inform target =<< fire
prop inputs@(Watched x : _) target fire = do
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
      waitForInputs ref (Watched y : ys) = do
        val <- readRef $ value y
        bot <- bottom
        if val == bot
          then do
            -- No usable input yet, wait until
            -- this cell gains input then check
            -- the rest
            listenToOnce (Prop ref) y
            backTrackingWriteRef ref (waitForInputs ref ys)
          else do
            -- This input has a usable value so
            -- go check the rest
            waitForInputs ref ys

  -- Create action ref with temp value
  propAction <- newRef (pure ())
  -- Override with proper action now that we have the ref
  writeRef propAction (waitForInputs propAction inputs)

  -- Have the first watched input trigger
  -- this propagator when it gains info
  listenToOnce (Prop propAction) x

  waitForInputs propAction inputs
