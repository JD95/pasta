{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Runtime.Prop where

import Control.Applicative (Alternative (empty))
import Control.Monad (filterM, forM_, join, unless)
import Data.Void
import Runtime.Ref
  ( GenTag (..),
    Ref (..),
    Strict (..),
    backTrackingModifyRef,
    backTrackingWriteRef,
  )

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

class Mergeable a where
  merge :: Old a -> New a -> Info a

class Bottom a where
  bottom :: a

class Top a where
  isTop :: a -> Bool

class (Mergeable a, Top a, Bottom a) => Lattice a

instance (Eq a) => Mergeable (Maybe a) where
  merge (Old Nothing) (New Nothing) = None
  merge (Old (Just _)) (New Nothing) = None
  merge (Old Nothing) (New (Just x)) = Gain (Just x)
  merge (Old (Just x)) (New (Just y))
    | x == y = None
    | otherwise = Conflict

instance Mergeable Void where
  merge (Old x) _ = absurd x

instance Bottom (Maybe a) where
  bottom = Nothing

instance Top (Maybe a) where
  isTop (Just _) = True
  isTop Nothing = False

instance Eq a => Lattice (Maybe a)

instance Mergeable () where
  merge (Old ()) (New ()) = None

instance Bottom () where
  bottom = ()

instance Top () where
  isTop _ = True

instance Lattice ()

instance (Mergeable a, Mergeable b) => Mergeable (a, b) where
  merge (Old (oldL, oldR)) (New (newL, newR)) =
    let l = merge (Old oldL) (New newL)
        r = merge (Old oldR) (New newR)
     in case (l, r) of
          (Gain upL, Gain upR) -> Gain (upL, upR)
          (Gain upL, None) -> Gain (upL, oldR)
          (None, Gain upR) -> Gain (oldL, upR)
          (None, None) -> None
          (_, _) -> Conflict

instance (Bottom a, Bottom b) => Bottom (a, b) where
  bottom = (bottom, bottom)

instance (Top a, Top b) => Top (a, b) where
  isTop (x, y) = isTop x && isTop y

instance (Top a, Bottom a, Top b, Bottom b, Mergeable a, Mergeable b) => Lattice (a, b)

data Watched m where
  Watched :: (Lattice a, Eq a, Ref m r) => Cell t m r a -> Watched m

data Keep = Keep | Remove

newtype Prop m r = Prop
  { action :: r (m ())
  }

data Cell t m r a where
  RawCell :: r a -> r [m (a -> Bool)] -> Cell t m r a
  TagCell :: r (t a, a) -> r [m (a -> Bool)] -> Cell t m r a

readCell :: (Ref m r) => Cell t m r a -> m a
readCell (RawCell ref _) = readRef ref
readCell (TagCell ref _) = snd <$> readRef ref

triggerWatchers :: Cell t m r a -> r [m (a -> Bool)]
triggerWatchers (RawCell _ trigs) = trigs
triggerWatchers (TagCell _ trigs) = trigs

backTrackingWriteCell :: (Alternative m, GenTag m t, Ref m r) => Cell t m r a -> a -> m ()
backTrackingWriteCell (RawCell ref _) x =
  backTrackingWriteRef ref x
backTrackingWriteCell (TagCell ref _) x = do
  tag <- genTag $ Strict x
  backTrackingWriteRef ref (tag, x)

cell :: forall r t a m. Ref m r => a -> m (Cell t m r a)
cell val = do
  RawCell <$> newRef val <*> newRef []

tagCell :: forall r t a m. (GenTag m t, Ref m r) => a -> m (Cell t m r a)
tagCell a = do
  tag <- genTag $ Strict a
  TagCell <$> newRef (tag, a) <*> newRef []

inform :: (Lattice a, Alternative m, GenTag m t, Ref m r) => Cell t m r a -> a -> m ()
inform target new = do
  old <- readCell target
  case merge (Old old) (New new) of
    Gain result -> do
      backTrackingWriteCell target result
      oldTriggers <- readRef (triggerWatchers target)
      -- Empty the list to avoid duplicates in
      -- the next steps
      writeRef (triggerWatchers target) []
      -- Fire all the propagators and keep the ones
      -- that want to stay active
      kept <- filterM ((<*> pure result)) oldTriggers
      -- So, in all, include the kept ones from last time
      -- and also the ones added from this round of
      -- propagation
      backTrackingModifyRef (triggerWatchers target) (kept <>)
    None -> pure ()
    Conflict -> empty

listenWhile ::
  (Alternative m, Ref m r1, Ref m r2) =>
  -- | Given the result, is this trigger needed anymore?
  (a -> Bool) ->
  Prop m r1 ->
  Cell t m r2 a ->
  m ()
listenWhile cond p target = do
  backTrackingModifyRef (triggerWatchers target) $
    (:) (cond <$ (join $ readRef $ action p))

listenToOnce :: (Alternative m, Ref m r1, Ref m r2) => Prop m r1 -> Cell t m r2 a -> m ()
listenToOnce = listenWhile (const False)

listenToAlways :: (Alternative m, Ref m r1, Ref m r2) => Prop m r1 -> Cell t m r2 a -> m ()
listenToAlways = listenWhile (const True)

prop :: (Eq (t a), Lattice a, Alternative m, GenTag m t, Ref m r) => [Watched m] -> Cell t m r a -> m a -> m ()
prop [] target fire = do
  inform target =<< fire
prop inputs (target :: Cell t m r a) fire = do
  -- Create action ref with temp value
  -- using the same type of ref as the
  -- target cell
  propAction <- newRef @m @r (pure ())

  let fireInto = do
        result <- fire
        case target of
          RawCell _ _ ->
            inform target result
          TagCell r _ -> do
            heldTag <- fst <$> readRef r
            resultTag <- genTag $ Strict result
            unless (heldTag == resultTag) $
              inform target result

      waitForInputs (Watched thisInput : ys) = do
        val <- readCell thisInput
        if val == bottom
          then do
            -- No usable input yet
            -- Change the action to wait for the rest of the inputs
            backTrackingWriteRef propAction (waitForInputs ys)
            -- And have it be called when thisInput fires,
            -- once it does, it *must* have usable input
            listenToOnce (Prop propAction) thisInput
          else do
            -- This input has a usable value so
            -- go check the rest
            waitForInputs ys
      waitForInputs [] = do
        forM_ inputs $ \(Watched input) ->
          -- Now that all the inputs have usable
          -- values, set them to trigger
          -- every on every update
          listenWhile (not . isTop) (Prop propAction) input
        backTrackingWriteRef propAction fireInto
        -- Fire the propagator for the first time
        fireInto

  -- Override with proper action now that we have the ref
  writeRef propAction (waitForInputs inputs)

  waitForInputs inputs
