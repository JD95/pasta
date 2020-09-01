{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logic.Propagator.PrimCell where

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.HashSet
import qualified Data.HashSet as Set
import Data.IORef
import Data.Primitive.MutVar
import Logic.Info
import Logic.Propagator.Class

data PrimCell m a = PrimCell (MutVar (PrimState m) (Info a, HashSet (Alert m)))

instance (Network m, PrimMonad m) => Inform m (PrimCell m) where
  inform x (PrimCell ref) = do
    (y, listeners) <- readMutVar ref
    case x <> y of
      NoInfo -> pure ()
      Info z -> do
        writeMutVar ref (Info z, listeners)
        forM_ listeners $ \(Alert _ go) -> go
      Contradiction -> error "Boom!"

instance (Network m, PrimMonad m) => Cell m (PrimCell m) where
  content (PrimCell ref) = fmap fst . readMutVar $ ref

instance (Network m, PrimMonad m) => HasTargets m (PrimCell m) where
  getTargets (PrimCell ref) = fmap (Set.toList . snd) . readMutVar $ ref
  addTarget t (PrimCell ref) = modifyMutVar ref $ second $ Set.insert t
  removeTarget i (PrimCell ref) = modifyMutVar ref $ second $ Set.delete (Alert i undefined)

newPrimCell :: (PrimMonad m) => forall a. m (PrimCell m a)
newPrimCell = fmap PrimCell $ newMutVar (NoInfo, Set.empty)
