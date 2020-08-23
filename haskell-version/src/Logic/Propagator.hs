{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Logic.Propagator where

import Control.Applicative
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Data.IORef
import Data.List
import Data.Sequence (Seq (..), (|>))
import Data.Vector.Mutable
import qualified Data.Vector.Mutable as Vec
import Logic.Info
import Unsafe.Coerce
import Prelude hiding (read)

newtype CellRef_ a = CellRef_ {unCell :: Int} deriving (Eq, Ord)

data Prop a where
  Cell :: Prop (CellRef_ a)
  Content_ :: CellRef_ a -> Prop (Info a)
  Inform :: Merge a => Info a -> CellRef_ a -> Prop ()
  Propagate :: Merge a => ([a] -> a) -> [CellRef_ a] -> CellRef_ a -> Prop ()
  Solve :: Prop ()

makeEffect ''Prop

data Heap a = Heap {heapSize :: !Int, heapData :: IOVector a}

heapInsert :: a -> Heap a -> IO (Int, Heap a)
heapInsert val heap = do
  let next = heapSize heap
  let len = Vec.length (heapData heap)
  data' <-
    if next == len
      then unsafeGrow (heapData heap) len
      else pure (heapData heap)
  Vec.unsafeWrite (heapData heap) next val
  pure $ (next, Heap (next + 1) data')

heapLookup :: Int -> Heap a -> IO a
heapLookup i (Heap _ values) = Vec.unsafeRead values i

heapWrite :: a -> Int -> Heap a -> IO ()
heapWrite val i heap = do
  Vec.unsafeWrite (heapData heap) i val

newtype PropRef_ = PropRef_ {unProp :: Int}

runProp ::
  (LastMember IO es, Merge a, Show a, Show b) =>
  IORef (Heap (Info a, [PropRef_])) ->
  IORef (Heap (IO ())) ->
  IORef (Seq PropRef_) ->
  Eff (Prop ': es) b ->
  Eff es b
runProp cellHeap propHeap alerts = interpretM $ \case
  Cell -> do
    (i, heap') <- heapInsert (NoInfo, []) =<< readIORef cellHeap
    writeIORef cellHeap heap'
    pure $ CellRef_ i
  Content_ (CellRef_ i) -> unsafeCoerce <$> getContent cellHeap i
  Inform inf c -> inform_ cellHeap alerts (unsafeCoerce inf) (unCell c)
  Propagate f inputs output -> do
    let action = do
          values <- traverse (getContent cellHeap . unCell) inputs
          print $ (fmap (const ())) <$> values
          let ins = foldr (liftA2 (:)) (Info []) values
          inform_ cellHeap alerts (unsafeCoerce $ f <$> ins) (unCell output)
    (actionRef, propHeap') <- heapInsert action =<< readIORef propHeap
    writeIORef propHeap propHeap'
    as <- readIORef alerts
    mapM_ (newNeighbor cellHeap alerts actionRef . unCell) inputs
    modifyIORef alerts $ \as -> as |> PropRef_ actionRef
    pure ()
  Solve -> do
    let go = do
          readIORef alerts >>= \case
            Empty -> pure ()
            (PropRef_ next :<| rest) -> do
              runNext <- heapLookup next =<< readIORef propHeap
              runNext
              go
    go
  where
    getContent cellHeap i = do
      (info, _) <- heapLookup i =<< readIORef cellHeap
      -- Will always be the correct type
      pure $ unsafeCoerce info

    inform_ ::
      (Show a, Merge a) =>
      -- | Cells
      IORef (Heap (Info a, [PropRef_])) ->
      -- | Alerts
      IORef (Seq PropRef_) ->
      Info a ->
      Int ->
      IO ()
    inform_ _ _ NoInfo _ = pure ()
    inform_ cellHeap alerts newInfo i = do
      (oldInfo, ns) <- heapLookup i =<< readIORef cellHeap
      case newInfo <> oldInfo of
        Info result -> do
          putStrLn $ "Learned " <> show i <> " = " <> show result
          heapWrite (result, ns) i . unsafeCoerce =<< readIORef cellHeap
          modifyIORef alerts $ \as -> foldl' (|>) as ns
        NoInfo -> pure ()
        Contradiction -> error $ "Contradiction: " <> show oldInfo <> " " <> show newInfo

    newNeighbor :: IORef (Heap (Info a, [PropRef_])) -> IORef (Seq PropRef_) -> Int -> Int -> IO ()
    newNeighbor cellHeap alerts pRef ref = do
      (vals, ns) <- heapLookup ref =<< readIORef cellHeap
      if any ((==) pRef . unProp) ns
        then pure ()
        else do
          heapWrite (vals, PropRef_ pRef : ns) ref =<< readIORef cellHeap
          modifyIORef alerts $ \as -> as |> PropRef_ pRef

runP :: Show a => Eff '[Prop, IO] a -> IO a
runP prog = do
  cellHeap <- newIORef =<< Heap 0 <$> new 100
  propHeap <- newIORef =<< Heap 0 <$> new 100
  alerts <- newIORef Empty
  runM . runProp @_ @Double cellHeap propHeap alerts $ prog

test :: IO ()
test = do
  result <- runP $ do
    x <- cell @Double
    y <- cell
    z <- cell
    propagate (\[in1, in2] -> in1 + in2) [x, y] z
    inform (Info 2.0) x
    send . print =<< content_ x
    inform (Info 3.0) y
    solve
    content_ z
  print result
