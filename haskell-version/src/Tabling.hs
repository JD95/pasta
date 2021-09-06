module Tabling where

import Dtsa (Dtsa, HasHole)
import qualified Dtsa

data Rel k a = Rel
  { table :: Dtsa k a,
    calc :: [k] -> Maybe a
  }

-- | Attempt to lookup an answer in the table first
-- if that fails then a new answer is calculated and
-- added into the table
query :: HasHole k => [k] -> Rel k a -> (Rel k a, [a])
query ks rel =
  case Dtsa.answers ks (table rel) of
    [] -> case calc rel ks of
      Just x -> (rel {table = Dtsa.insert ks x (table rel)}, [x])
      Nothing -> (rel, [])
    xs -> (rel, xs)

-- path x z :- path x y, edge y z
-- path x y :- edge x y

-- query: path _ _
--   No tabled results!
--   inserting
--   => path a? c?, edge c? b?
--      query: path _ _
--      Query in progress, fail!
--   => edge a? b?
