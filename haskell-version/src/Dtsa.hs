module Dtsa where

import Control.Monad
import qualified Data.List as List
import Data.Maybe
import Numeric.Natural
import Prelude hiding (length)

data Dtsa k a
  = Leaf a
  | Node [(k, Dtsa k a)]

query :: [k] -> Dtsa k a -> [a]
query = undefined

states :: Dtsa k a -> [Dtsa k a]
states x@(Leaf _) = [x]
states x@(Node more) = do
  xs <- states . snd <$> more
  x : xs

ith :: Natural -> Dtsa k a -> Maybe (Dtsa k a)
ith n xs = go n (states xs)
  where
    go _ [] = Nothing
    go 0 (x : xs) = Just x
    go n (x : xs) = go (n -1) xs

data Goal k = Goal [k]

step :: (k -> k -> Bool) -> k -> Dtsa k a -> [Dtsa k a]
step _ k (Leaf _) = []
step unify k (Node xs) = fmap snd . List.filter (unify k . fst) $ xs

desc :: (k -> k -> Bool) -> k -> (Dtsa k a, [Dtsa k a]) -> Maybe (Dtsa k a, [Dtsa k a])
desc unify k (this, cont) =
  case step unify k this of
    [] -> case cont of
      [] -> Nothing
      (x : xs) -> desc unify k (x, xs)
    (x : xs) -> Just (x, xs <> (step unify k =<< cont))

answers :: (k -> k -> Bool) -> Dtsa k a -> [k] -> [a]
answers unify init (k : ks) = go (desc unify k (init, [])) ks
  where
    val (Leaf x) = Just x
    val _ = Nothing
    go (Just (Leaf x, cont)) [] = [x] <> (catMaybes $ val <$> cont)
    go (Just this) (l : ls) = go (desc unify l this) ls
    go Nothing _ = []

merge :: Dtsa k a -> Dtsa k a -> Maybe (Dtsa k a)
merge (Leaf a) _ = Nothing
merge _ (Leaf a) = Nothing
merge (Node xs) (Node ys) = Just . Node $ xs <> ys

build :: [k] -> a -> Dtsa k a
build [] x = Leaf x
build (k : ks) x = Node [(k, build ks x)]

insert :: [k] -> a -> Dtsa k a -> Maybe (Dtsa k a)
insert ks x ys = merge ys (build ks x)

unifyC :: Char -> Char -> Bool
unifyC 'X' _ = True
unifyC _ 'X' = True
unifyC x y = x == y

test :: IO ()
test = do
  let dtsa =
        foldM merge (build "ab" 0) $
          [ build "ba" 1,
            build "ac" 2,
            build "bc" 3
          ]
  case dtsa of
    Just d -> print $ answers unifyC d "XX"
    Nothing -> putStrLn "Error in building dtsa"
