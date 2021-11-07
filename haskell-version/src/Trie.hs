module Trie (Trie, LookupError (..), lookup, insert, draw, empty) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tree (Tree (..), drawTree)
import Numeric.Natural
import Runtime.Ref
import Prelude hiding (lookup)

data Trie k a
  = Branch (Map k (Trie k a))
  | Leaf a

data LookupError
  = TooFewKeys
  | TooManyKeys
  | KeyMismatch

lookup :: Ord k => [k] -> Trie k a -> Either LookupError a
lookup [] (Branch _) = Left TooFewKeys
lookup (_ : _ : _) (Leaf _) = Left TooManyKeys
lookup [] (Leaf x) = Right x
lookup (k : ks) (Branch b) =
  case Map.lookup k b of
    Just child -> lookup ks child
    Nothing -> Left KeyMismatch

insert :: Ord k => [k] -> a -> Trie k a -> Trie k a
insert (k : []) x (Branch b) = Branch $ Map.insert k (Leaf x) b
insert (k : ks) x (Branch b) =
  let inner = case Map.lookup k b of
        Just child -> child
        Nothing -> Branch Map.empty
   in Branch $ Map.insert k (insert ks x $ inner) b

empty :: Trie k a
empty = Branch Map.empty

toStringTree :: String -> (k -> String) -> (a -> String) -> Trie k a -> Tree String
toStringTree node _ drawA (Leaf x) = Node node [Node (drawA x) []]
toStringTree node drawK drawA (Branch b) =
  Node node $
    fmap (\(k, child) -> toStringTree (drawK k) drawK drawA child) $
      Map.assocs b

draw :: String -> (k -> String) -> (a -> String) -> Trie k a -> String
draw root drawK drawA trie = drawTree $ toStringTree root drawK drawA trie
