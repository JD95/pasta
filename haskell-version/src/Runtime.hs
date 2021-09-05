{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Runtime where

import Control.Monad
import Data.Functor.Foldable (cata)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Debug.Trace
import Numeric.Natural
import Runtime.Dsl
import Runtime.Log
import Runtime.Ref
import Runtime.Types
import Prelude hiding (const, id, log)

rtMatch :: RtValF r -> Match -> Bool
rtMatch _ MAny = True
rtMatch (RtPrimF (RtInt i)) (MInt j) = i == j
rtMatch (RtConF tag _) (MCon i) = tag == i

check :: RtValF r -> [(Match, RtCtl)] -> RtCtl
check switch xs = case filter (rtMatch switch . fst) xs of
  [] -> error "Non-exhaustive pattern match"
  ((_, p) : _) -> p

rtEval :: (Log m, Ref m r) => r (RtClo r) -> Vector (r (RtClo r)) -> m (r (RtClo r))
rtEval ref frame = do
  readRef ref >>= \case
    (RtThunk x frees) -> do
      resultRef <- go (RtEnv frame frees) x
      writeRef ref =<< readRef resultRef
      pure resultRef
    (RtProp target refN srcs action) -> do
      readRef refN >>= \case
        0 -> do
          -- Unwrap all the cells so the
          -- action can work with the values
          -- directly
          propFrame <- for srcs $ \s -> do
            -- 0 = No Info
            -- 1 = Partial Info
            -- 2 = Top Covered (Any more info -> contradiction)
            -- 3 = Contradiction
            readRef s >>= \case
              RtWhnf (RtConF 1 r) -> pure r
              RtWhnf (RtConF 2 r) -> pure r
              _ -> pure s
          go (RtEnv (Vec.cons target propFrame) mempty) action
        n -> do
          readRef (srcs Vec.! fromIntegral n) >>= \case
            RtWhnf (RtConF 0 _) -> pure ref
            RtWhnf (RtConF 1 _) -> modifyRef refN (\m -> m - 1) *> pure ref
    val -> pure ref
  where
    go :: (Log m, Ref m r) => RtEnv r -> RtCtl -> m (r (RtClo r))
    -- Allocations
    go env (RtAllocThunk f frees) = newRef . RtThunk f =<< traverse (go env) frees
    go env (RtAllocProd xs) = newRef . RtWhnf . RtProdF =<< traverse (go env) xs
    go env (RtAllocCon tag x) = newRef . RtWhnf . RtConF tag =<< go env x
    go env (RtAllocPrim x) = newRef . RtWhnf . RtPrimF $ x
    go env (RtAllocCell merge) = do
      cellRef <- go env rtInfoEmpty
      newRef $ RtCell cellRef merge mempty
    go env (RtAllocProp target n srcs action) = do
      target' <- go env target
      srcs' <- traverse (go env) srcs
      refN <- newRef n
      -- Schedule propagator
      newRef $ RtProp target' refN srcs' action
    -- Variable Access
    go env (RtFreeVar i) = pure $ rtFrees env Vec.! fromIntegral i
    go env (RtStackVar i) = do
      let frame = rtStackFrame env
      if fromIntegral i >= Vec.length frame
        then do
          log =<< rtDisplayEnv env
          error $ concat ["Out of bounds stack access '", show i, "'"]
        else pure $ frame Vec.! fromIntegral i
    -- Indexing
    go env (RtIndex x i) = do
      go env x >>= readRef >>= \case
        (RtWhnf (RtProdF xs)) -> pure $ xs Vec.! fromIntegral i
        (RtWhnf _) -> error "Can only index a product!"
        (RtThunk _ _) -> error "Cannot index a thunk!"
    -- Control Flow
    go env (RtApp f args) = do
      -- Evaluate new stack frame
      frame <- traverse (go env) args
      let env' = env {rtStackFrame = frame}
      log $ concat ["Applying ", show f, " to ", show args]
      log =<< rtDisplayEnv env'
      -- Construct thunk for function
      -- with current environment
      log $ rtDisplayCtl f
      ref <- go env f
      -- Push new stack frame and evaluate thunk
      rtEval ref frame
    -- Sequential Evaluation
    go env (RtEval x next) = do
      log $ "Evaling " <> show x
      ref <- go env x
      _ <- rtEval ref (rtStackFrame env)
      go env next
    go env (RtBranch x alts) = do
      log =<< rtDisplayEnv env
      log $ "Branching on " <> rtDisplayCtl x
      go env x >>= readRef >>= \case
        RtWhnf x' -> do
          let pick = check x' $ Vec.toList alts
          log $ "Picked branch: " <> rtDisplayCtl pick
          go env pick
    go env (RtAddCellDep cell prop) = do
      log $ "Adding cell dep"
      cellRef <- go env cell
      readRef cellRef >>= \case
        RtCell valRef merge deps -> do
          propRef <- go env prop
          readRef propRef >>= \case
            RtProp _ _ _ _ -> pure ()
            _ -> error "Adding non-prop as dependent of cell!"
          writeRef cellRef . RtCell valRef merge $ Vec.cons propRef deps
        _ -> error "Adding prop dependent to a non-cell!"
      pure cellRef
    go env (RtInformCell cell newValue) = do
      log $ "Informing cell"
      c <- go env cell
      readRef c >>= \case
        RtCell cellRef merge dependants -> do
          -- Evaluate the given expression down to
          -- a single value. Could be Logic
          newValueRef <- go env newValue
          merged <- go (env {rtStackFrame = Vec.fromList [cellRef, newValueRef]}) merge
          -- eval to whnf so tag can be inspected
          result <- readRef =<< rtEval merged mempty

          -- check result for failure
          case result of
            RtWhnf (RtConF 3 _) -> error "Merge failure! Backtracking unimplemented!"
            -- So long as there isn't a merge failure, the result can have any kind
            -- of distance. Which means merge can return a Logic value
            _ -> pure ()

          -- update cell
          writeRef cellRef result

          -- trigger propagators that rely on
          -- this cell
          sequence_ $ flip rtEval mempty <$> dependants

          -- The only thing that makes sense to return here
          -- is the reference to the cell itself
          -- The informs will be part of a sequence
          pure c

normalize :: (Log m, Ref m r) => r (RtClo r) -> m RtVal
normalize ref =
  readRef ref >>= \case
    (RtWhnf (RtPrimF x)) -> pure $ RtPrim x
    (RtWhnf (RtConF tag x)) -> RtCon tag <$> normalize x
    (RtWhnf (RtProdF xs)) -> RtProd <$> traverse normalize xs
    (RtThunk _ _) -> normalize =<< rtEval ref Vec.empty

rtDisplayCtl :: RtCtl -> String
rtDisplayCtl x = go x
  where
    go (RtApp f xs) = "(" <> go f <> ") " <> intercalate " " (Vec.toList $ parens . go <$> xs)
    go (RtStackVar i) = "#" <> show i
    go (RtAllocPrim x) = show x
    go (RtAllocThunk f frees) = concat ["(", go f, ") {", intercalate " " (Vec.toList $ parens . go <$> frees), "}"]
    go x = show x

rtDisplay :: RtClo r -> String
rtDisplay (RtWhnf (RtPrimF (RtInt i))) = show i
rtDisplay (RtWhnf (RtConF tag _)) = "Con " <> show tag <> " _"
rtDisplay (RtThunk _ _) = "Thunk"
rtDisplay (RtCell _ _ _) = "Cell"
rtDisplay (RtProp _ _ _ _) = "Prop"

rtDisplay' :: (Log m, Ref m r) => r (RtClo r) -> m String
rtDisplay' x = cata go <$> normalize x
  where
    go (RtPrimF (RtInt i)) = show i
    go (RtConF tag x) = show "Con " <> show tag <> " " <> x
    go (RtProdF xs) = concat ["(", intercalate ", " (Vec.toList xs), ")"]

rtDisplayEnv :: Ref m r => RtEnv r -> m String
rtDisplayEnv env = do
  let dis = fmap (intercalate " " . Vec.toList) . traverse (fmap (parens . rtDisplay) . readRef)
  frame <- dis $ rtStackFrame env
  frees <- dis $ rtFrees env
  pure $ concat [replicate 8 '-', "\nStack Frame: {" <> frame, "}\nFree Vars: {", frees, "}"]

parens :: String -> String
parens x = "(" <> x <> ")"

test1 :: IO ()
test1 = do
  let id = rtThunk [] $ rtVar 0
  let const = rtVar 0
  fx <- newRef . rtBox $ id `rtApp` [rtInt 2]
  y <- newRef . rtBox $ rtInt 1
  code <- newRef $ rtBox const
  putStrLn . ("fx before: " <>) . rtDisplay =<< readRef fx
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef code (Vec.fromList [fx, y])
  putStrLn . ("fx after: " <>) . rtDisplay =<< readRef fx

test2 :: IO ()
test2 = do
  let snd = RtEval (rtVar 0) (RtIndex (rtVar 0) 1)
  prd <- newRef . rtBox $ rtProd [rtInt 1, rtInt 2]
  code <- newRef $ rtBox snd
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef code (Vec.fromList [prd])

test3 :: IO ()
test3 = do
  let flip =
        rtCase 0 $
          [ (MInt 0, RtAllocPrim $ RtInt 1),
            (MInt 1, RtAllocPrim $ RtInt 0)
          ]
  x <- newRef . rtBox $ rtInt 0
  code <- newRef $ rtBox flip
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef code (Vec.fromList [x])

test4 :: IO ()
test4 = do
  let isOne =
        rtCase 0 $
          [ (MInt 1, true),
            (MAny, false)
          ]
  x <- newRef . rtBox $ rtInt 0
  code <- newRef $ rtBox isOne
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef code (Vec.fromList [x])

test5 :: IO ()
test5 = do
  -- not = \x -> case x of
  --  True -> False
  --  False -> True
  let not =
        rtThunk [] $
          rtCase 0 $
            [ (MCon 0, false),
              (MCon 1, true)
            ]
  code <- newRef . rtBox $ not `rtApp` [true]
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef code Vec.empty

test6 :: IO ()
test6 = do
  -- const = \x y -> x
  let const = rtThunk [] $ rtVar 0
  -- flip = \f y x -> f x y
  let flip = rtThunk [] $ rtVar 0 `rtApp` [rtVar 2, rtVar 1]
  -- code = (\f y x -> f x y) (\x y -> x) 2 1
  code <- newRef . rtBox $ flip `rtApp` [const, rtInt 2, rtInt 1]
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef code Vec.empty

test7 :: IO ()
test7 = do
  -- const = \x y -> x
  let const = rtThunk [] $ rtVar 0
  -- func = \y f x -> f x y
  let func = rtThunk [] $ rtVar 1 `rtApp` [rtVar 2, rtVar 0]
  -- code = (\y f x -> f x y) 2 (\x y -> x) 1
  code <- newRef . rtBox $ func `rtApp` [rtInt 2, const, rtInt 1]
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef code Vec.empty

-- | Access to free variables works
test8 :: IO ()
test8 = do
  -- func = \y f x -> f x y
  func <- newRef . rtBox $ rtVar 1 `rtApp` [rtVar 2, rtVar 0]
  -- const = \x y -> x
  const <- newRef . rtBox $ rtVar 0
  -- code = func 2 const 1
  code <- newRef $ RtThunk (rtFree 0 `rtApp` [rtInt 2, rtFree 1, rtInt 1]) (Vec.fromList [func, const])
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef code Vec.empty

newEmptyCell c = join $ rtEval @IO @IORef <$> newRef (rtBox c) <*> pure mempty

test :: IO ()
test = do
  -- Setup the cell
  let xCell = rtCell identityLat
  let yCell = rtCell identityLat
  x <- newEmptyCell xCell
  y <- newEmptyCell yCell
  -- Inform the cell
  code <-
    newRef . rtBox $
      rtSeq
        [ RtAddCellDep (rtVar 0) $ rtProp (rtVar 0) [rtVar 0] idProp,
          rtInformCell (rtVar 0) (rtInt 5)
        ]
  void $ rtEval @IO @IORef code (Vec.fromList [x, y])
