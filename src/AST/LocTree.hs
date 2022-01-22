module AST.LocTree
  ( AST.LocTree.lookup,
    AST.LocTree.fold,
    AST.LocTree.foldM,
    AST.LocTree.transform,
    AST.LocTree.spine,
    LocTree (..),
    mkLocTree,
  )
where

import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Functor.Foldable

-- | Annotates a recursive structure with the segment of the source text it occupies
data LocTree l f = LocTree {locStart :: l, locEnd :: l, locContent :: f (LocTree l f)}

-- | A smart constructor for LocTree which ensures locStart <= locEnd
mkLocTree :: Ord l => l -> l -> f (LocTree l f) -> Maybe (LocTree l f)
mkLocTree x y inner = LocTree x y inner <$ guard (x <= y)

-- | Given a location, lookup the smallest subtree with the range that contains the location
lookup :: (Foldable f, Ord l) => l -> LocTree l f -> Maybe (LocTree l f)
lookup l top@(LocTree x y inner) =
  if hasL top
    then case filter hasL $ toList inner of
      [] -> Just top
      [x] -> Just x
      (x : _ : _) -> error "LocTree Constraint Broken! Multiple branches with same range!"
    else Nothing
  where
    hasL (LocTree x y _) = x <= l && l <= y

-- | Fold with the range
fold :: Functor f => (l -> l -> f a -> a) -> LocTree l f -> a
fold f (LocTree x y inner) = f x y (AST.LocTree.fold f <$> inner)

foldM :: (Traversable f, Applicative m) => (l -> l -> f (m a) -> m a) -> LocTree l f -> m a
foldM f (LocTree x y inner) = f x y (foldM f <$> inner)

transform :: (Traversable f, Monad m) => (l -> l -> f (m (LocTree l g)) -> m (g (LocTree l g))) -> LocTree l f -> m (LocTree l g)
transform f (LocTree x y inner) = do
  this <- f x y $ transform f <$> inner
  pure $ LocTree x y this

spine :: (Corecursive f) => LocTree l (Base f) -> f
spine (LocTree _ _ inner) = embed $ spine <$> inner
