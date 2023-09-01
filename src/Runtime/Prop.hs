{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Runtime.Prop
  ( Watched (..),
    Cell (..),
    Pull,
    Inform (..),
    Ref (..),
    FailAction (..),
    refCell,
    unify,
    inform,
    informPeaceful,
    pushBase,
  )
where

import Control.Applicative (Alternative (empty))
import Control.Monad (forM_, join)
import Data.Traversable
import Lattice

data Watched m where
  Watched :: (Lattice a) => Inform cell m a -> cell -> Watched m

-- | A cell that updates via pushes from others
-- Will wait until all inputs have some information
-- before tracking updates
data Cell m a where
  Cell :: (Inform cell m a) -> cell -> Cell m a

-- | When a cell value is updated, various triggers
-- will also fire. The triggers perform things like firing
-- other propagators.
--
-- The predicate returned determines whether or not
-- this trigger remains active after it fires, based
-- on the value of the updated cell
--
-- See `listenWhile` for usage
newtype Trigger m a = Trigger {runTrigger :: m (a -> Bool)}

newtype Prop m r = Prop
  { action :: r (m ())
  }

-- | A cell that updates via pulls on reads
-- For things that are expensive to merge on
-- every update from the inputs
data Pull m r a = Pull (r a) (m a) [r Bool] (r [Trigger m a])

{-
class Value cell where
  value :: (Lattice a) => cell -> m a

instance Value Cell where
  value (Cell ref _) = readRef ref

instance Value Pull where
  value (Pull ref go srcs deps) = do
    oldValue <- readRef ref
    inputsHaveChanged <- fmap or . for srcs $ \src -> do
      -- Check to see if input value
      -- has updated since last read
      flag <- readRef src
      backTrackingWriteRef src False
      pure flag
    if inputsHaveChanged
      then do
        result <- merge (Old oldValue) . New <$> go
        case result of
          Gain newValue -> do
            -- Save this value for the
            -- next read. If none of the
            -- inputs change, then no need
            -- to recalculate anything
            backTrackingWriteRef ref newValue
            _ <- fireAll newValue [] =<< readRef deps
            pure newValue
          None -> pure oldValue
          Conflict -> empty
      else pure oldValue

-}

refCell :: forall a m r. Monad m => a -> Ref m r -> m (Cell m a)
refCell val Ref {..} = do
  x <- newRef val
  trig <- newRef []
  let info = Inform (readRef . snd) (writeRef . snd) (readRef . fst) (writeRef . fst)
  pure $ Cell info (x, trig)

data Inform cell m a = Inform
  { readWatchers :: cell -> m [Trigger m a],
    writeWatchers :: cell -> [Trigger m a] -> m (),
    readValue :: cell -> m a,
    writeValue :: cell -> a -> m ()
  }

newtype FailAction m = FailAction {runFailAction :: m ()}

informBase :: (Monad m, Lattice a) => Bool -> cell -> a -> FailAction m -> Inform cell m a -> m ()
informBase isPeaceful target new f Inform {..} = do
  old <- readValue target
  case merge (Old old) (New new) of
    Gain result -> do
      writeValue target result
      kept <- fireAll result [] =<< readWatchers target
      writeWatchers target kept
    None -> do
      pure ()
    Conflict ->
      if isPeaceful then pure () else runFailAction f

fireAll :: Monad m => a -> [Trigger m a] -> [Trigger m a] -> m [Trigger m a]
fireAll _ kept [] = pure kept
fireAll result kept (t : ts) = do
  keepTriggerGiven <- runTrigger t
  next <-
    if keepTriggerGiven result
      then pure $ t : kept
      else pure kept
  fireAll result ts next

-- | Merges info with cell, conflicts causing branch failures
inform :: (Monad m, Lattice a) => cell -> a -> FailAction m -> Inform cell m a -> m ()
inform = informBase False

-- | Merges info with cell, treating conflicts as no-ops
informPeaceful :: (Monad m, Lattice a) => cell -> a -> FailAction m -> Inform cell m a -> m ()
informPeaceful = informBase True

listenWhile :: Monad m => (a -> Bool) -> Prop m r -> cell -> Inform cell m a -> Ref m r -> m ()
listenWhile cond p cell Inform {..} Ref {..} = do
  others <- readWatchers cell
  let new = do
        -- Fire the given propagator by
        -- pull the action it's action
        -- and immediately run it
        join $ readRef (action p)
        -- After the propagator fires
        -- use this condition to check
        -- if this trigger should stay
        -- active
        pure cond
  writeWatchers cell (Trigger new : others)

listenToOnce :: Monad m => Prop m r -> cell -> Inform cell m a -> Ref m r -> m ()
listenToOnce = listenWhile (const False)

data Ref m r = Ref
  { readRef :: forall x. r x -> m x,
    writeRef :: forall x. r x -> x -> m (),
    newRef :: forall x. x -> m (r x)
  }

-- | Creates a push relationship between the watched cells and the target cell
pushBase :: (Monad m, Lattice a) => [Watched m] -> cell -> m a -> FailAction m -> Inform cell m a -> Ref m r -> m ()
pushBase [] target fire fail i _ = do
  x <- fire
  inform target x fail i
pushBase inputs@(Watched (xInfo) x : _) target fire fail i r@(Ref {..}) = do
  -- Create action ref with temp value
  propAction <- newRef (pure ())
  -- Override with proper action now that we have the ref
  writeRef propAction (waitForInputs propAction inputs)

  -- Have the first watched input trigger
  -- this propagator when it gains info
  listenToOnce (Prop propAction) x xInfo r

  waitForInputs propAction inputs
  where
    fireInto = do
      result <- fire
      inform target result fail i

    waitForInputs ref [] = do
      -- Now that all the inputs have usable
      -- values, fire the propagator now and
      -- every time one of them updates
      forM_ inputs $ \(Watched wInfo w) ->
        -- However, once an input is saturated
        -- with information, we can stop listening
        -- it will only ever learn nothing or a contradiction
        listenWhile (not . isTop) (Prop ref) w wInfo r
      writeRef ref fireInto
      fireInto
    waitForInputs ref (Watched yInfo y : ys) = do
      val <- (readValue yInfo) y
      if LatOrd val <= LatOrd bottom
        then do
          -- No usable input yet, wait until
          -- this cell gains input then check
          -- the rest
          listenToOnce (Prop ref) y yInfo r
          writeRef ref (waitForInputs ref ys)
        else do
          -- This input has a usable value so
          -- go check the rest
          waitForInputs ref ys

unify ::
  (Monad m, Lattice a) =>
  cell1 ->
  Inform cell1 m a ->
  cell2 ->
  Inform cell2 m a ->
  FailAction m ->
  Ref m r ->
  m ()
unify x xInfo y yInfo fail r = do
  pushBase [] y (readValue xInfo x) fail yInfo r
  pushBase [] x (readValue yInfo y) fail xInfo r

{-
-- | Creates a pull relationship from the inputs into the cell
pull :: (Alternative m, Lattice a, MonadRef m) => [Watched m (Ref m)] -> m a -> m (Pull m (Ref m) a)
pull inputs go = do
  sources <- for inputs $ \(Watched input) -> do
    flag <- newRef False
    backTrackingModifyRef (getWatchers input) $ \others ->
      let new = do
            writeRef flag True
            -- Keep this notification
            -- until this input saturates
            pure (not . isTop)
       in Trigger new : others
    pure flag
  ref <- newRef bottom
  watchers <- newRef []
  pure $ Pull ref go sources watchers

pmap ::
  (Alternative m, MonadRef m, Lattice a, Lattice b) =>
  (a -> b) ->
  Cell m (Ref m) a ->
  Cell m (Ref m) b ->
  m ()
pmap f x result = do
  push [Watched x] result $
    f <$> value x

rel ::
  (Alternative m, MonadRef m, Lattice a, Lattice b) =>
  (a -> b) ->
  (b -> a) ->
  Cell m (Ref m) a ->
  Cell m (Ref m) b ->
  m ()
rel f g x y = do
  push [Watched x] y $
    f <$> value x
  push [Watched y] x $
    g <$> value y

-- | Helpful when applying things like `map`
-- where there is a single "output" value
--
-- >>> x <- cell (Just 1)
-- >>> y <- apply $ map (1 +) x
apply ::
  (Alternative m, MonadRef m, Lattice a) =>
  (Cell m (Ref m) a -> m ()) ->
  m (Cell m (Ref m) a)
apply make = do
  result <- cell bottom
  make result
  pure result

end ::
  (MonadRef m) =>
  (Cell m (Ref m) a -> m (Cell m (Ref m) a))
end = pure

(|>) ::
  (Lattice a, Lattice b, Lattice c, Alternative m, MonadRef m) =>
  (Cell m (Ref m) a -> Cell m (Ref m) b -> m ()) ->
  (Cell m (Ref m) b -> m (Cell m (Ref m) c)) ->
  (Cell m (Ref m) a -> m (Cell m (Ref m) c))
(|>) f g x = g =<< apply (f x)

infixr 1 |>

peaceful ::
  (Lattice a, Alternative m, MonadRef m) =>
  Cell m (Ref m) a ->
  Cell m (Ref m) a ->
  m (Cell m (Ref m) a)
peaceful other this = do
  thisP <- apply $ pmap Peaceful this
  otherP <- apply $ pmap Peaceful other
  push [Watched otherP] thisP $ value thisP
  apply $ pmap unPeaceful thisP
-}
