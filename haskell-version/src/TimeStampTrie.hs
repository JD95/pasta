{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

newtype TimeStamp = TimeStamp {unTimeStamp :: Natural}
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num)

data BackRef r a = BackRef {prev :: Maybe (r (BackRef r a)), value :: a}

instance Eq a => Eq (BackRef r a) where
  x == y = value x == value y

instance Ord a => Ord (BackRef r a) where
  x <= y = value x <= value y

data TimeStampTrie r k = TimeStampTrie
  { rootTime :: TimeStamp,
    keyIndex :: Trie (BackRef r k) (BackRef r k, TimeStamp),
    timeIndex :: IntMap (BackRef r k)
  }

insert :: (Ord k, Ref m r) => NonEmpty k -> TimeStampTrie r k -> m (TimeStampTrie r k)
insert ks ts = do
  linked <- linkKeys ks Nothing []
  let newTime = rootTime ts + 1
  let end = NE.last linked
  let front = case NE.init linked of
        [] -> end :| []
        (x : xs) -> x :| xs
  pure $
    ts
      { rootTime = newTime,
        keyIndex = Trie.insert front (end, newTime) (keyIndex ts),
        timeIndex = IntMap.insert (fromIntegral $ unTimeStamp newTime) end $ timeIndex ts
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

test :: IO ()
test = do
  let drawStamped x = show (value x)
  tst :: TimeStampTrie IORef Char <-
    ( insert ('b' :| "ae")
        <=< insert ('b' :| "oo")
        <=< insert ('b' :| "ee")
      )
      =<< pure empty
  putStrLn $ Trie.draw "root" (show . value) (\(b, TimeStamp t) -> show (value b) <> ":" <> show t) (keyIndex tst)
  print =<< lookupTime (TimeStamp 2) tst
