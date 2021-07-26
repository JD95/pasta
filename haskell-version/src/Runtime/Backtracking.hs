{-# LANGUAGE KindSignatures #-}

module Runtime.Backtracking where

import Data.Kind
import Runtime.Ref

data BackItem (r :: Type -> Type) (a :: Type) = BackItem
  { target :: r a,
    value :: a
  }

data BackSt r a = BackSt
  { forward :: [BackItem r a],
    backward :: [BackItem r a]
  }

data Direction = Back | Forward

apply :: (Ref m r) => BackItem r a -> m (BackItem r a)
apply bi = do
  prev <- readRef (target bi)
  writeRef (target bi) (value bi)
  pure $ BackItem (target bi) prev

backtrackProp :: (Ref m r) => BackSt r a -> m ()
backtrackProp = go Forward
  where
    done :: Monad m => m ()
    done = pure ()

    go :: (Ref m r) => Direction -> BackSt r a -> m ()
    go Forward st@(BackSt [] _) = go Back st
    go Forward (BackSt (f : fs) bs) = do
      b <- apply f
      go Forward (BackSt fs (b : bs))
    go Back (BackSt _ []) = done
    go Back (BackSt fs (b : bs)) = do
      f <- apply b
      go Back (BackSt (f : fs) bs)
