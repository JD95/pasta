{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Runtime.Prop
  ( Watched (..),
    Cell,
    Pull,
    Value (..),
    Inform (..),
    cell,
    inform,
    informPeaceful,
    push,
    pull,
    pmap,
    rel,
    unify,
    apply,
    (|>),
    end,
    peaceful,
  )
where

import Control.Applicative (Alternative (empty))
import Control.Monad (forM_, join)
import Data.Traversable
import Lattice
import Runtime.Ref
  ( MonadRef (..),
    backTrackingModifyRef,
    backTrackingWriteRef,
  )

data Watched m r where
  Watched :: (Value f, Inform f, Lattice a) => f m r a -> Watched m r

-- | A cell that updates via pushes from others
-- Will wait until all inputs have some information
-- before tracking updates
data Cell m r a
  = Cell
      (r a)
      -- ^ Reference to the value held in the cell
      (r [Trigger m a])

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

class Value cell where
  value :: (Lattice a, Alternative m, MonadRef m) => cell m (Ref m) a -> m a

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

cell :: forall a m. MonadRef m => a -> m (Cell m (Ref m) a)
cell val = do
  x <- newRef val
  trig <- newRef []
  pure $ Cell x trig

class Inform cell where
  getWatchers :: cell m r a -> r [Trigger m a]
  getValueRef :: cell m r a -> r a

instance Inform Cell where
  getWatchers (Cell _ ws) = ws
  getValueRef (Cell r _) = r

instance Inform Pull where
  getWatchers (Pull _ _ _ ws) = ws
  getValueRef (Pull r _ _ _) = r

informBase :: (Inform cell, Lattice a, Alternative m, MonadRef m) => Bool -> cell m (Ref m) a -> a -> m ()
informBase isPeaceful target new = do
  let ref = getValueRef target
  let watchers = getWatchers target
  old <- readRef ref
  case merge (Old old) (New new) of
    Gain result -> do
      backTrackingWriteRef ref result
      kept <- fireAll result [] =<< readRef watchers
      backTrackingWriteRef watchers kept
    None -> do
      pure ()
    Conflict ->
      if isPeaceful then pure () else empty

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
inform :: (Lattice a, Alternative m, MonadRef m) => Cell m (Ref m) a -> a -> m ()
inform = informBase False

-- | Merges info with cell, treating conflicts as no-ops
informPeaceful :: (Lattice a, Alternative m, MonadRef m) => Cell m (Ref m) a -> a -> m ()
informPeaceful = informBase True

listenWhile :: (Inform f, Alternative m, MonadRef m) => (a -> Bool) -> Prop m (Ref m) -> f m (Ref m) a -> m ()
listenWhile cond p f =
  backTrackingModifyRef (getWatchers f) $ \others ->
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
     in Trigger new : others

listenToOnce :: (Inform f, Alternative m, MonadRef m) => Prop m (Ref m) -> f m (Ref m) a -> m ()
listenToOnce = listenWhile (const False)

-- | Creates a push relationship between the watched cells and the target cell
push :: (Lattice a, Alternative m, MonadRef m) => [Watched m (Ref m)] -> Cell m (Ref m) a -> m a -> m ()
push [] target fire = inform target =<< fire
push inputs@(Watched x : _) target@(Cell _ _) fire = do
  -- Create action ref with temp value
  propAction <- newRef (pure ())
  -- Override with proper action now that we have the ref
  writeRef propAction (waitForInputs propAction inputs)

  -- Have the first watched input trigger
  -- this propagator when it gains info
  listenToOnce (Prop propAction) x

  waitForInputs propAction inputs
  where
    fireInto = do
      result <- fire
      inform target result

    waitForInputs ref [] = do
      -- Now that all the inputs have usable
      -- values, fire the propagator now and
      -- every time one of them updates
      forM_ inputs $ \(Watched w) ->
        -- However, once an input is saturated
        -- with information, we can stop listening
        -- it will only ever learn nothing or a contradiction
        listenWhile (not . isTop) (Prop ref) w
      backTrackingWriteRef ref fireInto
      fireInto
    waitForInputs ref (Watched y : ys) = do
      val <- value y
      if LatOrd val <= LatOrd bottom
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

unify ::
  (Alternative m, MonadRef m, Lattice a) =>
  Cell m (Ref m) a ->
  Cell m (Ref m) a ->
  m ()
unify x y = do
  push [] y $ value x
  push [] x $ value y

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
