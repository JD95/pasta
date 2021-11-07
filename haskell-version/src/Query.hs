{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Query () where

import Control.Monad
import Control.Monad.State
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

data QueryResult m
  = -- | action to unify answer with query and next result
    QuerySuccess (m ()) (m (QueryResult m))
  | -- | No answer at the moment, but more possible
    QuerySuspend
  | -- | All answers have been retrieved
    QueryComplete
  | -- | Bad Query
    QueryError

query :: (Ord k, Ref m r) => [k] -> r (Predicate m r k) -> m (QueryResult m)
query ks predRef = do
  pred <- readRef predRef
  let thisFrame = lookupSubgoal ks <$> readRef (subgoals pred)
  thisFrame >>= \case
    Right frame -> case answers frame of
      Generator _ ->
        -- It's not clear exactly what this means
        --
        -- if a generator ends up calling itself
        -- that would mean no more answers could be
        -- achieved, because it would go through the
        -- same resolution with the same variables
        --
        -- For now the above assumption is taken
        pure QueryComplete
      Consumer _ -> consumeFrom thisFrame
      Loader _ -> loadFrom thisFrame
    Left Trie.KeyMismatch -> do
      -- Allocate a new Generator
      modifyRef (subgoals pred)
        . addSubgoal ks
        $ SubgoalFrame (TimeStamp 0) [] [] (Generator (answerTrie pred))
      -- Run clause resolution, may involve recursive calls to same predicate
      let (x :| rest) = clauses pred
      let xs = foldr (generateFrom thisFrame) (pure QueryComplete) rest
      pure $ QuerySuccess x xs
    Left _ -> pure QueryError

-- | Runs clause resolution to get answers, until it either completes
-- or is converted into a consumer
generateFrom ::
  Ref m r =>
  m (Either Trie.LookupError (SubgoalFrame r k)) ->
  m () ->
  m (QueryResult m) ->
  m (QueryResult m)
generateFrom lookupFrame current next = do
  answer <- current
  -- Check if frame has turned from generator to consumer/loader
  lookupFrame >>= \case
    Right frame -> case answers frame of
      Generator tstRef -> do
        -- insert answer into tst
        -- gather pending-answers if necessary
        next
      Consumer _ -> consumeFrom lookupFrame
      Loader _ -> loadFrom lookupFrame

-- | Does not run clause resolution, merely consumes answers from
-- some generator
consumeFrom ::
  Ref m r =>
  m (Either Trie.LookupError (SubgoalFrame r k)) ->
  m (QueryResult m)
consumeFrom = undefined

-- | Like consuming, but will only ever complete. It never suspends
loadFrom ::
  Ref m r =>
  m (Either Trie.LookupError (SubgoalFrame r k)) ->
  m (QueryResult m)
loadFrom = undefined

data Predicate m r k = Predicate
  { subgoals :: r (SubgoalTrie r k),
    answerTrie :: r (TimeStampTrie k),
    -- | The various ways to generate new answers
    clauses :: NonEmpty (m ())
  }

newtype TimeStamp = TimeStamp {unTimeStamp :: Natural}
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num)

data Stamped a = Stamped {time :: TimeStamp, value :: a}
  deriving (Show)

instance Eq a => Eq (Stamped a) where
  x == y = value x == value y

instance Ord a => Ord (Stamped a) where
  x <= y = value x <= value y

-- TODO: This isn't quite right for the TST
-- need capability to quickly lookup based on
-- both key and timestamp independently
--
-- Also, need to update the time stamps
-- on the path down, currently only the
-- unique tail will have the correct
-- timestamp
data TimeStampTrie k = TimeStampTrie
  {rootTime :: TimeStamp, tst :: Trie (Stamped k) ()}

newtype SubgoalTrie r k
  = SubgoalTrie (Trie k (SubgoalFrame r k))

lookupSubgoal :: (Ord k) => [k] -> SubgoalTrie r k -> Either Trie.LookupError (SubgoalFrame r k)
lookupSubgoal ks (SubgoalTrie sgt) = Trie.lookup ks sgt

addSubgoal :: Ord k => [k] -> SubgoalFrame r k -> SubgoalTrie r k -> SubgoalTrie r k
addSubgoal ks sgf (SubgoalTrie sgt) = SubgoalTrie $ Trie.insert ks sgf sgt

data SubgoalFrame r k = SubgoalFrame
  { timeStamp :: TimeStamp,
    answerList :: [TimeStamp],
    pendingAnswers :: [TimeStamp],
    answers :: FrameType r k
  }

data FrameType r k
  = Generator (r (TimeStampTrie k))
  | Consumer (r (SubgoalFrame r k))
  | Loader (r (SubgoalFrame r k))

checkCompleted :: Ref m r => SubgoalFrame r k -> m Bool
checkCompleted = undefined

-- Convert consumer frame into loader
complete :: Ref m r => r (SubgoalFrame r k) -> m ()
complete = undefined

test :: IO ()
test = do
  let drawStamped x = show (value x) <> ":" <> show (unTimeStamp $ time x)
  let trie =
        Trie.insert (Stamped 1 <$> "bed") ()
          . Trie.insert (Stamped 2 <$> "bad") ()
          . Trie.insert (Stamped 3 <$> "bee") ()
          $ Trie.empty
  putStrLn $ Trie.draw "root" drawStamped show trie
