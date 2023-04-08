{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Runtime.Logic.Partial where

import Data.Bifunctor
import Runtime.Prop

newtype Partial h a = Partial {unPartial :: Maybe (h (Partial h a) a)}

class EqH h where
  eqH :: (Eq a, Eq b) => h b a -> h b a -> Bool

instance (Eq a, EqH h) => Eq (Partial h a) where
  (Partial Nothing) == (Partial Nothing) = True
  (Partial Nothing) == (Partial (Just _)) = False
  (Partial (Just _)) == (Partial Nothing) = False
  (Partial (Just x)) == (Partial (Just y)) = eqH x y

class MergeableH h where
  mergeH :: (Mergeable b, Mergeable a) => Old (h b a) -> New (h b a) -> Info (h b a)

class TopH h where
  isTopH :: (Lattice b, Lattice a) => h b a -> Bool

instance (MergeableH h, Mergeable a, Eq a) => Mergeable (Partial h a) where
  merge (Old (Partial Nothing)) (New (Partial Nothing)) = None
  merge (Old (Partial (Just _))) (New (Partial Nothing)) = None
  merge (Old (Partial Nothing)) (New (Partial (Just new))) = Gain (Partial $ Just new)
  merge (Old (Partial (Just x))) (New (Partial (Just y))) =
    Partial . Just <$> mergeH (Old x) (New y)

instance Bottom (Partial h a) where
  bottom = Partial Nothing

instance (Eq a, Lattice a, MergeableH h, TopH h) => Top (Partial h a) where
  isTop (Partial Nothing) = False
  isTop (Partial (Just x)) = isTopH x

instance (MergeableH h, TopH h, Eq a, Lattice a) => Lattice (Partial h a)
