{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Runtime where

import Control.Monad
import Data.Functor.Foldable (Fix (..), cata)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Numeric.Natural
import Prelude hiding (const, id, log)

data Match
  = MInt Int
  | MCon Natural
  | MAny
  deriving (Show)

data RtCtl
  = -- | Check for path, branch
    RtBranch RtCtl (Vector (Match, RtCtl))
  | -- | Evaluate second with the side effects
    -- of evaluating the first
    RtEval RtCtl RtCtl
  | -- | Index into a product
    RtIndex RtCtl Natural
  | -- | Push args onto stack and eval func
    RtApp RtCtl (Vector RtCtl)
  | -- | Allocate an Empty cell
    RtAllocCell
      RtCtl
  | -- | Inform cell using given value
    RtInformCell
      RtCtl
      -- ^ Cell to inform
      RtCtl
      -- ^ Value
  | -- | Allocate a new closure
    RtAllocThunk RtCtl (Vector RtCtl)
  | -- | Allocate a new product
    RtAllocProd (Vector RtCtl)
  | -- | Allocate a new tagged value
    RtAllocCon Natural RtCtl
  | -- | Allocate a primitive value
    RtAllocPrim PrimVal
  | -- | Reference to a variable on the stack frame
    RtStackVar Natural
  | -- | Reference to a free variable
    RtFreeVar Natural
  deriving (Show)

data RtClo r
  = -- | An unevaluated thunk, result of application
    RtCell
      (r (RtClo r))
      -- ^ The cell, holding the data
      RtCtl
      -- ^ How to merge information
      (Vector (r (RtClo r)))
      -- ^ The propagators to trigger if this cell updates
  | RtThunk
      RtCtl
      -- ^ Eval Code
      (Vector (r (RtClo r)))
      -- ^ Captured Free Variables
  | -- | An evaluated thunk
    RtWhnf (RtVal (r (RtClo r)))

data PrimVal
  = RtInt Int
  deriving (Show, Eq)

data RtVal a
  = RtPrim PrimVal
  | RtProd (Vector a)
  | RtCon Natural a
  deriving (Functor)

data RtEnv r = RtEnv {rtStackFrame :: Vector (r (RtClo r)), rtFrees :: Vector (r (RtClo r))}

class Monad m => Ref m r where
  newRef :: a -> m (r a)
  readRef :: r a -> m a
  writeRef :: r a -> a -> m ()

instance Ref IO IORef where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

class Monad m => Log m where
  log :: String -> m ()

instance Log IO where
  log = putStrLn

rtMatch :: RtVal r -> Match -> Bool
rtMatch _ MAny = True
rtMatch (RtPrim (RtInt i)) (MInt j) = i == j
rtMatch (RtCon tag _) (MCon i) = tag == i

check :: RtVal r -> [(Match, RtCtl)] -> RtCtl
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
    val -> pure ref
  where
    go :: (Log m, Ref m r) => RtEnv r -> RtCtl -> m (r (RtClo r))
    -- Allocations
    go env (RtAllocThunk f frees) = newRef . RtThunk f =<< traverse (go env) frees
    go env (RtAllocProd xs) = newRef . RtWhnf . RtProd =<< traverse (go env) xs
    go env (RtAllocCon tag x) = newRef . RtWhnf . RtCon tag =<< go env x
    go env (RtAllocPrim x) = newRef . RtWhnf . RtPrim $ x
    go env (RtAllocCell merge) = do
      cellRef <- go env rtInfoEmpty
      newRef $ RtCell cellRef merge mempty
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
        (RtWhnf (RtProd xs)) -> pure $ xs Vec.! fromIntegral i
        (RtWhnf _) -> error "Can only index a product!"
        (RtThunk _ _) -> error "Cannot index a thunk!"
    -- Control Flow
    go env (RtApp f args) = do
      -- Evaluate new stack frame
      frame <- traverse (go env) args
      let env' = env {rtStackFrame = frame}
      log =<< rtDisplayEnv env'
      -- Construct thunk for function
      -- with current environment
      log $ rtDisplayCtl f
      ref <- go env f
      -- Push new stack frame and evaluate thunk
      rtEval ref frame
    -- Sequential Evaluation
    go env (RtEval x next) = do
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
    go env (RtInformCell cell newValue) = do
      c <- go env cell
      readRef c >>= \case
        RtCell cellRef merge targets -> do
          newValueRef <- go env newValue
          merged <- go (env {rtStackFrame = Vec.fromList [cellRef, newValueRef]}) merge
          -- eval to whnf so tag can be inspected
          result <- readRef =<< rtEval merged mempty

          -- check result for failure
          case result of
            RtWhnf (RtCon 3 _) -> error "Merge failure! Backtracking unimplemented!"
            _ -> pure ()

          -- update cell
          writeRef cellRef result

          -- TODO: trigger target propagators
          -- The only thing that makes sense to return here
          -- is the reference to the cell itself
          -- The informs will be part of a sequence
          pure c

normalize :: (Log m, Ref m r) => r (RtClo r) -> m (Fix RtVal)
normalize ref =
  readRef ref >>= \case
    (RtWhnf (RtPrim x)) -> pure . Fix $ RtPrim x
    (RtWhnf (RtCon tag x)) -> Fix . RtCon tag <$> normalize x
    (RtWhnf (RtProd xs)) -> Fix . RtProd <$> traverse normalize xs
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
rtDisplay (RtWhnf (RtPrim (RtInt i))) = show i
rtDisplay (RtWhnf (RtCon tag _)) = "Con " <> show tag <> " _"
rtDisplay (RtThunk _ _) = "Thunk"

rtDisplay' :: (Log m, Ref m r) => r (RtClo r) -> m String
rtDisplay' x = cata go <$> normalize x
  where
    go (RtPrim (RtInt i)) = show i
    go (RtCon tag x) = show "Con " <> show tag <> " " <> x
    go (RtProd xs) = concat ["(", intercalate ", " (Vec.toList xs), ")"]

rtDisplayEnv :: Ref m r => RtEnv r -> m String
rtDisplayEnv env = do
  let dis = fmap (intercalate " " . Vec.toList) . traverse (fmap (parens . rtDisplay) . readRef)
  frame <- dis $ rtStackFrame env
  frees <- dis $ rtFrees env
  pure $ concat [replicate 8 '-', "\nStack Frame: {" <> frame, "}\nFree Vars: {", frees, "}"]

parens :: String -> String
parens x = "(" <> x <> ")"

rtCase :: Natural -> [(Match, RtCtl)] -> RtCtl
rtCase x = RtEval (RtStackVar x) . RtBranch (RtStackVar x) . Vec.fromList

rtInt :: Int -> RtCtl
rtInt = RtAllocPrim . RtInt

rtProd :: [RtCtl] -> RtCtl
rtProd = RtAllocProd . Vec.fromList

rtIndex :: RtCtl -> Natural -> RtCtl
rtIndex = RtIndex

rtCon :: Natural -> RtCtl -> RtCtl
rtCon = RtAllocCon

rtApp :: RtCtl -> [RtCtl] -> RtCtl
rtApp x = RtApp x . Vec.fromList

rtThunk :: [RtCtl] -> RtCtl -> RtCtl
rtThunk xs x = RtAllocThunk x $ Vec.fromList xs

rtCell :: RtCtl -> RtCtl
rtCell = RtAllocCell

rtInformCell :: RtCtl -> RtCtl -> RtCtl
rtInformCell = RtInformCell

rtInfoEmpty :: RtCtl
rtInfoEmpty = RtAllocCon 0 unit

rtInfoPartial :: RtCtl -> RtCtl
rtInfoPartial = RtAllocCon 1

rtInfoTop :: RtCtl -> RtCtl
rtInfoTop = RtAllocCon 2

rtInfoConflict :: RtCtl
rtInfoConflict = RtAllocCon 3 unit

rtBox :: RtCtl -> RtClo r
rtBox x = RtThunk x (Vec.fromList [])

rtVar :: Natural -> RtCtl
rtVar = RtStackVar

rtFree :: Natural -> RtCtl
rtFree = RtFreeVar

firstTopMerge :: RtCtl
firstTopMerge =
  rtCase
    0 -- Match on the existing cell info
    [ -- If it's empty, fill with the
      -- new value and mark as Top
      (MCon 0, rtInfoTop $ rtVar 1),
      -- If there is any information
      -- already in the cell, raise a
      -- conflict
      (MAny, rtInfoConflict)
    ]

unit :: RtCtl
unit = RtAllocProd $ Vec.fromList []

true :: RtCtl
true = RtAllocCon 0 unit

false :: RtCtl
false = RtAllocCon 1 unit

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
