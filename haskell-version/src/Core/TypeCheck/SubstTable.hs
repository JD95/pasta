module SubstTable where

import Prelude hiding (lookup, elem)
import Data.Map.Strict
import Data.Functor.Foldable

import Subst

newtype SubstTable key value = MkSubstTable (Map key value)

rewrite :: (Ord key , Subst f key) => key -> Fix f -> SubstTable key (Fix f) -> SubstTable key (Fix f)
rewrite target val (MkSubstTable tbl) =
  let tbl' = alter (const $ Just val) target tbl
  in MkSubstTable $ subst val target <$> tbl'

lookupSubst :: Ord key => key -> SubstTable key a -> Maybe a
lookupSubst key (MkSubstTable tbl) = lookup key tbl
