{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Range where

import AST.Tree
import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Functor.Classes
import Data.Functor.Foldable
import Lens.Micro.Platform
import Parsing.Lexer

-- TODO: Rewrite LocTree logic to work against the new Tree type
data Range = Range {_start :: RowCol, _end :: RowCol}
  deriving (Show, Eq, Ord)

makeLenses ''Range

class HasRange a where
  range :: Lens' a Range

instance HasRange t => HasRange (Tree t f) where
  range = lens (view (ctx . range)) (flip $ set (ctx . range))

prepend :: HasRange t => RowCol -> Tree t f -> Tree t f
prepend x tree =
  if x < (tree ^. ctx . range . start)
    then tree & range . start .~ x
    else tree

append :: HasRange t => RowCol -> Tree t f -> Tree t f
append x tree =
  if tree ^. ctx . range . end < x
    then tree & ctx . range . end .~ x
    else tree

-- | Given a location, lookup the smallest subtree with the range that contains the location
lookup :: (HasRange t, Foldable f) => RowCol -> Tree t f -> Maybe (Tree t f)
lookup l top =
  if hasL top
    then case filter hasL $ toList $ top ^. branches of
      [] -> Just top
      [x] -> Just x
      (_ : _ : _) -> error "LocTree Constraint Broken! Multiple branches with same range!"
    else Nothing
  where
    hasL (Tree t _) = t ^. range . start <= l && l <= t ^. range . end
