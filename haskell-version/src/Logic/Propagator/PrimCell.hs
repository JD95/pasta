{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Logic.Propagator.PrimCell where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Primitive
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Primitive.MutVar
import Logic.Info
import Logic.Propagator.Class

data PrimCell m a = PrimCell (MutVar (PrimState m) (Info a, HashSet (Alert m)))

instance (Network m, PrimMonad m) => Inform m (PrimCell m) where
  inform new (PrimCell ref) = do
    (old, listeners) <- readMutVar ref
    case old <> new of
      NoInfo -> pure ()
      Info z -> do
        backtrackWrite ref (Info z, listeners)
        forM_ listeners $ \(Alert _ go) -> go
      Contradiction -> empty

instance (Network m, PrimMonad m) => Cell m (PrimCell m) where
  content (PrimCell ref) = fmap fst . readMutVar $ ref

instance (Network m, PrimMonad m) => HasTargets m (PrimCell m) where
  getTargets (PrimCell ref) = fmap (Set.toList . snd) . readMutVar $ ref
  addTarget t (PrimCell ref) = modifyMutVar ref $ second $ Set.insert t
  removeTarget i (PrimCell ref) = modifyMutVar ref $ second $ Set.delete (Alert i undefined)

newPrimCell :: (PrimMonad m) => forall a. m (PrimCell m a)
newPrimCell = fmap PrimCell $ newMutVar (NoInfo, Set.empty)

backtrackWrite :: (MonadPlus m, PrimMonad m) => MutVar (PrimState m) a -> a -> m ()
backtrackWrite ref val = do
  unwind ((),) (writeMutVar ref) $ atomicModifyMutVar ref (val,)

backtrackModify :: (MonadPlus m, PrimMonad m) => MutVar (PrimState m) a -> (a -> a) -> m ()
backtrackModify ref f = do
  unwind ((),) (writeMutVar ref) $ atomicModifyMutVar ref $ \a -> (f a, a)

unwind ::
  MonadPlus m =>
  (a -> (b, c)) ->
  (c -> m d) ->
  m a ->
  m b
unwind f mu na =
  na >>= \a -> case f a of
    (b, c) -> pure b <|> (mu c *> empty)
