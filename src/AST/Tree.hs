{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Tree where

import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Functor.Classes
import Data.Functor.Foldable
import Lens.Micro.Platform

data Tree t f = Tree {_ctx :: t, _branches :: f (Tree t f)}

makeLenses ''Tree

data TreeF t f a = TreeF {_ctxF :: t, _branchesF :: f a}
  deriving (Functor)

makeLenses ''TreeF

type instance Base (Tree t f) = TreeF t f

instance Functor f => Recursive (Tree l f) where
  project (Tree t x) = TreeF t x

instance Functor f => Corecursive (Tree l f) where
  embed (TreeF t x) = Tree t x

instance (Show1 f, Show l) => Show (Tree l f) where
  show (Tree _ _) = undefined

instance (Eq1 f, Eq l) => Eq (Tree l f) where
  (Tree _ _) == (Tree _ _) = undefined

spine :: (Corecursive f) => Tree t (Base f) -> f
spine (Tree _ inner) = embed $ spine <$> inner
