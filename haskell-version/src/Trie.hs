module Trie (Trie, LookupError (..), lookup, insert, draw, empty) where

import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
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
  deriving (Show)

data LookupError
  = TooFewKeys
  | TooManyKeys
  | KeyMismatch

lookup :: Ord k => NonEmpty k -> Trie k a -> Either LookupError a
lookup (_ :| _) (Leaf _) = Left TooManyKeys
lookup (k :| ks) (Branch b) =
  case (,) ks <$> Map.lookup k b of
    Just ([], Branch _) -> Left TooFewKeys
    Just (next : more, Branch child) -> lookup (next :| more) (Branch child)
    Just ([], Leaf x) -> Right x
    Just (_ : _, Leaf x) -> Left TooManyKeys
    Nothing -> Left KeyMismatch

insert :: Ord k => NonEmpty k -> a -> Trie k a -> Trie k a
insert (k :| (next : more)) x (Branch b) =
  Branch $ Map.alter go k b
  where
    insertInto = Just . insert (next :| more) x
    go Nothing = insertInto $ Branch Map.empty
    go (Just child) = insertInto child
insert (k :| []) x (Branch b) = Branch $ Map.insert k (Leaf x) b
insert _ _ (Leaf _) = error "Insert into Trie with too many keys"

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
