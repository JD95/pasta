module Dtsa
  ( Dtsa,
    HasHole (..),
    empty,
    insert,
    answers,
    merge,
    build,
  )
where

import Control.Monad
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Numeric.Natural
import Prelude hiding (length)

-- | In spirit this implements the DTSA from
-- "A Thread in Time Saves Tabling Time", but
-- this is not a 1-1 implementation.
data Dtsa k a
  = Leaf a
  | Node (Map k [Dtsa k a])

class Ord a => HasHole a where
  hole :: a

unify :: HasHole a => a -> a -> Bool
unify x y = x == y || x == hole || y == hole

-- | Rather than travel down one branch at a time
-- and calling a `next` function to get a sibling,
-- all siblings are collected at once
step :: HasHole k => k -> Dtsa k a -> [Dtsa k a]
step k (Leaf _) = []
step k (Node xs)
  | k == hole = concat $ Map.elems xs
  | otherwise = concat $ catMaybes [Map.lookup k xs, Map.lookup hole xs]

-- | While the original uses a continuation stack to put off
-- the work of finding subsequent answers, here this is emulated
-- by keeping a lazy list of processing the siblings.
--
-- In theory this means we won't spend time finding more answers
-- if only the first n are required.
desc :: HasHole k => k -> (Dtsa k a, [Dtsa k a]) -> Maybe (Dtsa k a, [Dtsa k a])
desc k (this, cont) =
  case step k this of
    [] -> case cont of
      [] -> Nothing
      (x : xs) -> desc k (x, xs)
    (x : xs) -> Just (x, xs <> (step k =<< cont))

answers :: HasHole k => [k] -> Dtsa k a -> [a]
answers (k : ks) init = go (desc k (init, [])) ks
  where
    val (Leaf x) = Just x
    val _ = Nothing
    go (Just (Leaf x, cont)) [] = [x] <> (catMaybes $ val <$> cont)
    go (Just this) (l : ls) = go (desc l this) ls
    go Nothing _ = []

-- | Invalid merges are ignored
merge :: Ord k => Dtsa k a -> Dtsa k a -> Dtsa k a
merge (Leaf a) _ = Leaf a
merge _ (Leaf a) = Leaf a
merge (Node xs) (Node ys) = Node $ Map.unionWith (<>) xs ys

build :: [k] -> a -> Dtsa k a
build [] x = Leaf x
build (k : ks) x = Node $ Map.singleton k [build ks x]

insert :: Ord k => [k] -> a -> Dtsa k a -> Dtsa k a
insert ks x ys = merge ys (build ks x)

empty :: Dtsa k a
empty = Node Map.empty
