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

backtrackProp :: (Ref m r) => BackSt r a -> m ()
backtrackProp = go Forward
  where
    done :: Monad m => m ()
    done = pure ()

    go :: (Ref m r) => Direction -> BackSt r a -> m ()
    go Forward st@(BackSt [] _) = go Back st
    go Forward (BackSt (f : fs) bs) = do
      prev <- readRef (target f)
      -- inform target f (value f)
      writeRef (target f) (value f)
      let b = BackItem (target f) prev
      go Forward (BackSt fs (b : bs))
    go Back (BackSt _ []) = done
    go Back (BackSt fs (b : bs)) = do
      prev <- readRef (target b)
      writeRef (target b) (value b)
      let f = BackItem (target b) prev
      go Back (BackSt (f : fs) bs)
