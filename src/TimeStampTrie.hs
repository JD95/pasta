{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TimeStampTrie where

import Control.Monad
import Control.Monad.State
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable
import Numeric.Natural
import Runtime.Ref
import Trie (Trie)
import qualified Trie as Trie

data TimeStampTrie r k = TimeStampTrie
  { rootTime :: TimeStamp,
    -- | Stores the keys of the trie with parent references
    -- which are used when looking up times via `timeIndex`.
    keyIndex :: Trie (BackRef r k) (TimeStamp),
    -- | Maps a time to the end of a string of keys, from
    -- which the rest of the string can be collected.
    timeIndex :: IntMap (BackRef r k)
  }

insert :: (Ord k, Ref m r) => NonEmpty k -> TimeStampTrie r k -> m (TimeStampTrie r k)
insert ks ts = do
  linked <- linkKeys ks Nothing []
  let newTime = rootTime ts + 1
  pure $
    ts
      { rootTime = newTime,
        keyIndex = Trie.insert linked newTime (keyIndex ts),
        timeIndex = IntMap.insert (fromIntegral $ unTimeStamp newTime) (NE.last linked) $ timeIndex ts
      }
  where
    linkKeys :: Ref m r => NonEmpty k -> Maybe (r (BackRef r k)) -> [BackRef r k] -> m (NonEmpty (BackRef r k))
    linkKeys (k :| []) prev revRefs =
      pure $ NE.reverse $ BackRef prev k :| revRefs
    linkKeys (k :| (next : more)) prev revRefs = do
      let this = BackRef prev k
      link <- newRef this
      linkKeys (next :| more) (Just link) (this : revRefs)

lookupTime :: Ref m r => TimeStamp -> TimeStampTrie r k -> m (Maybe (NonEmpty k))
lookupTime (TimeStamp t) tst = do
  traverse (gather []) $ IntMap.lookup (fromIntegral t) (timeIndex tst)
  where
    gather :: Ref m r => [k] -> BackRef r k -> m (NonEmpty k)
    gather ks (BackRef ref k) = do
      case ref of
        Just prev -> do
          next <- readRef prev
          gather (k : ks) next
        Nothing -> pure $ k :| ks

empty :: TimeStampTrie r k
empty = TimeStampTrie 0 Trie.empty IntMap.empty

newtype TimeStamp = TimeStamp {unTimeStamp :: Natural}
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num)

data BackRef r a = BackRef {prev :: Maybe (r (BackRef r a)), value :: a}

instance Eq a => Eq (BackRef r a) where
  x == y = value x == value y

instance Ord a => Ord (BackRef r a) where
  x <= y = value x <= value y

instance Show a => Show (BackRef r a) where
  show (BackRef _ a) = show a

test :: IO ()
test = do
  let drawStamped x = show (value x)
  tst <-
    ( insert ('b' :| "ee")
        <=< insert ('b' :| "ed")
      )
      =<< pure (empty @IORef)
  putStrLn $ Trie.draw "root" (show . value) (show . unTimeStamp) (keyIndex tst)
