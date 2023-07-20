{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Tree where

import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Functor.Classes
import Data.Functor.Foldable

data Tree t f = Tree {treeValue :: t, treeBranches :: f (Tree t f)}

data TreeF t f a = TreeF {treeValueF :: t, treeBranchesF :: f a}
  deriving (Functor)

type instance Base (Tree t f) = TreeF t f

instance Functor f => Recursive (Tree l f) where
  project (Tree t x) = TreeF t x

instance Functor f => Corecursive (Tree l f) where
  embed (TreeF t x) = Tree t x

instance (Show1 f, Show l) => Show (Tree l f) where
  show (Tree _ _) = undefined

instance (Eq1 f, Eq l) => Eq (Tree l f) where
  (Tree _ _) == (Tree _ _) = undefined
