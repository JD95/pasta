module Tabling where

import Dtsa (Dtsa)
import qualified Dtsa

data Rel k a = Rel
  { table :: Dtsa k a,
    calc :: [k] -> Maybe a
  }

-- | Attempt to lookup an answer in the table first
-- if that fails then a new answer is calculated and
-- added into the table
query :: (k -> k -> Bool) -> [k] -> Rel k a -> (Rel k a, [a])
query match ks rel =
  case Dtsa.answers match (table rel) ks of
    [] -> case calc rel ks of
      Just x -> (rel {table = Dtsa.insert ks x (table rel)}, [x])
      Nothing -> (rel, [])
    xs -> (rel, xs)
