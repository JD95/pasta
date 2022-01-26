{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Query where

import Data.List.NonEmpty (NonEmpty (..))
import Runtime.Ref
import TimeStampTrie
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

query :: (Ord k, Ref m r) => NonEmpty k -> r (Predicate m r k) -> m (QueryResult m)
query ks predRef = do
  p <- readRef predRef
  lookupSubgoal ks <$> readRef (subgoals p) >>= \case
    Right frame ->
      answers <$> readRef frame >>= \case
        -- Another generator is already doing
        -- the work, so just consume it's answers
        Generator _ -> consumeFrom frame
        -- This sub-goal has been subsumed so
        -- start consuming answers from the
        -- subsumptive goal
        Consumer gen -> consumeFrom gen
    Left Trie.KeyMismatch -> do
      -- Allocate a new Generator
      frame <- newRef $ SubgoalFrame (TimeStamp 0) [] [] (Generator (answerTrie p))
      modifyRef (subgoals p) . addSubgoal ks $ frame
      -- Run clause resolution, may involve recursive calls to same predicate
      let (x :| rest) = clauses p
      let xs = foldr (generateFrom frame) (pure QueryComplete) rest
      pure $ QuerySuccess x xs
    Left _ -> pure QueryError

-- | Corresponds a "Generator Node"
-- Runs clause resolution to get answers, until it either completes
-- or is converted into a consumer
generateFrom ::
  Ref m r =>
  r (SubgoalFrame r k) ->
  m () ->
  m (QueryResult m) ->
  m (QueryResult m)
generateFrom frame _ next = do
  -- answer <- current
  -- Check if frame has turned from generator to consumer/loader
  answers <$> readRef frame >>= \case
    Generator _ -> do
      -- insert answer into tst
      -- gather pending-answers if necessary
      next
    Consumer _ -> consumeFrom frame

-- | Corresponds to "Consumer Nodes"
-- Does not run clause resolution, merely consumes answers from
-- some generator
consumeFrom ::
  Ref m r =>
  r (SubgoalFrame r k) ->
  m (QueryResult m)
consumeFrom = undefined

data Predicate m r k = Predicate
  { subgoals :: r (SubgoalTrie r k),
    answerTrie :: r (TimeStampTrie r k),
    -- | The various ways to generate new answers
    clauses :: NonEmpty (m ())
  }

newtype SubgoalTrie r k
  = SubgoalTrie (Trie k (r (SubgoalFrame r k)))

lookupSubgoal :: (Ord k) => NonEmpty k -> SubgoalTrie r k -> Either Trie.LookupError (r (SubgoalFrame r k))
lookupSubgoal ks (SubgoalTrie sgt) = Trie.lookup ks sgt

addSubgoal :: Ord k => NonEmpty k -> r (SubgoalFrame r k) -> SubgoalTrie r k -> SubgoalTrie r k
addSubgoal ks sgf (SubgoalTrie sgt) = SubgoalTrie $ Trie.insert ks sgf sgt

data SubgoalFrame r k = SubgoalFrame
  { timeStamp :: TimeStamp,
    answerList :: [TimeStamp],
    pendingAnswers :: [TimeStamp],
    answers :: FrameType r k
  }

data FrameType r k
  = Generator (r (TimeStampTrie r k))
  | Consumer (r (SubgoalFrame r k))

checkCompleted :: Ref m r => SubgoalFrame r k -> m Bool
checkCompleted = undefined

-- Convert consumer frame into loader
complete :: Ref m r => r (SubgoalFrame r k) -> m ()
complete = undefined
