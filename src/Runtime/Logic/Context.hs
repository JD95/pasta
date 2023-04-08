module Runtime.Logic.Context where

-- import Control.Monad (foldM)
import Data.Map (Map)

-- import qualified Data.Map as Map
-- import Runtime.Prop

newtype Context k a = Context {unContext :: Map k a}

{--
instance Bottom (Context k a) where
  bottom = Context Map.empty

instance Top (Context k a) where
  isTop = const False

data ContextMergeResult a
  = NoUpdate
  | NewEntries
  | Conflicts [a]

instance (Ord k, Eq a) => Mergeable (Context k a) where
  merge (Old old) (New new) =
    case foldr go (NoUpdate, unContext old) (Map.assocs $ unContext new) of
      (NoUpdate, _) -> None
      (NewEntries, ctx) -> Gain (Context ctx)
      (Conflicts _, _) -> Conflict
    where
      go (key, new) (result, oldCtx) =
        case Map.lookup key oldCtx of
          Just old ->
            if old == new
              then [

              --}
