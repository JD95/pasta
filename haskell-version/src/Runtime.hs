{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Runtime () where

import Data.Functor.Foldable (Fix(..), cata) 
import Data.List
import Control.Monad
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Numeric.Natural
import Prelude hiding (const, id, log)

data Match
  = MInt Int
  | MCon Natural
  | MAny
    deriving Show

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
    deriving Show

data RtClo r
  = -- | An unevaluated thunk, result of application
    RtThunk
      -- | Eval Code
      RtCtl
      -- | Captured Free Variables
      (Vector (r (RtClo r)))
  | -- | An evaluated thunk
    RtWhnf (RtVal (r (RtClo r)))

data PrimVal
  = RtInt Int
    deriving Show

data RtVal a
  = RtPrim PrimVal
  | RtProd (Vector a) 
  | RtCon Natural a
    deriving Functor

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

rtEval :: (Log m, Ref m r) => RtClo r -> Vector (r (RtClo r)) -> m (RtClo r)
rtEval (RtThunk x frees) frame = readRef =<< go (RtEnv frame frees) x
  where
    go :: (Log m, Ref m r) => RtEnv r -> RtCtl -> m (r (RtClo r))
    -- Allocations
    go env (RtAllocThunk f frees) = do
      newRef . RtThunk f =<< traverse (go env) frees
    go env (RtAllocProd xs) =
      newRef . RtWhnf . RtProd =<< traverse (go env) xs
    go env (RtAllocCon tag x) =
      newRef . RtWhnf . RtCon tag =<< go env x
    go env (RtAllocPrim x) = newRef . RtWhnf . RtPrim $ x
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
      let env' = env { rtStackFrame = frame }
      log =<< rtDisplayEnv env'
      -- Construct thunk for function
      -- with current environment
      log $ rtDisplayCtl f
      f' <- readRef =<< go env f 
      -- Push new stack frame and evaluate thunk
      newRef =<< rtEval f' frame
    go env (RtEval x next) = do
      ref <- go env x
      val <- readRef ref
      writeRef ref =<< rtEval val (rtStackFrame env)
      go env next
    go env (RtBranch x alts) = do
      go env x >>= readRef >>= \case
        RtWhnf x' ->
          -- Create a new thunk with access to the frees from this
          -- closure
          newRef $ RtThunk (check x' $ Vec.toList alts) (rtFrees env)
    go env (RtFreeVar i) = pure $ rtFrees env Vec.! fromIntegral i
    go env (RtStackVar i) = pure $ rtStackFrame env Vec.! fromIntegral i
rtEval r _ = pure r

normalize :: (Log m, Ref m r) => RtClo r -> m (Fix RtVal)
normalize (RtWhnf (RtPrim x)) = pure . Fix $ RtPrim x
normalize (RtWhnf (RtCon tag x)) = Fix . RtCon tag <$> (normalize =<< readRef x)
normalize thunk = normalize =<< rtEval thunk Vec.empty

rtDisplayCtl :: RtCtl -> String
rtDisplayCtl x = go x where
  go (RtApp f xs) = "(" <> go f <> ") " <> intercalate " " (Vec.toList $ parens . go <$> xs)
  go (RtStackVar i) = "#" <> show i
  go (RtAllocPrim x) = show x
  go (RtAllocThunk f frees) = concat ["(", go f, ") {", intercalate " " (Vec.toList $ parens . go <$> frees), "}"] 
  go x = show x

rtDisplay :: RtClo r -> String
rtDisplay (RtWhnf (RtPrim (RtInt i))) = show i
rtDisplay (RtWhnf (RtCon tag _)) = "Con " <> show tag <> " _"
rtDisplay (RtThunk _ _) = "Thunk"

rtDisplay' :: (Log m, Ref m r) => RtClo r -> m String
rtDisplay' x = cata go <$> normalize x where
  go (RtPrim (RtInt i)) = show i
  go (RtCon tag x) = show "Con " <> show tag <> " " <> x

rtDisplayEnv :: Ref m r => RtEnv r -> m String
rtDisplayEnv env = do
  let dis = fmap (intercalate " " . Vec.toList) . traverse (fmap (parens . rtDisplay) . readRef)
  frame <- dis $ rtStackFrame env
  frees <- dis $ rtFrees env
  pure $ concat [replicate 8 '-', "\nStack Frame: {" <>  frame, "}\nFree Vars: {", frees, "}"]

parens :: String -> String
parens x = "(" <> x <> ")"

rtCase :: Natural -> [(Match, RtCtl)] -> RtCtl
rtCase x = RtEval (RtStackVar x) . RtBranch (RtStackVar x) . Vec.fromList

rtInt :: Int -> RtCtl
rtInt = RtAllocPrim . RtInt

rtProd :: [RtCtl] -> RtCtl
rtProd = RtAllocProd . Vec.fromList

rtApp :: RtCtl -> [RtCtl] -> RtCtl
rtApp x = RtApp x . Vec.fromList

rtThunk :: [RtCtl] -> RtCtl -> RtCtl
rtThunk xs x = RtAllocThunk x $ Vec.fromList xs

rtBox :: RtCtl -> RtClo r
rtBox x = RtThunk x (Vec.fromList [])

rtVar :: Natural -> RtCtl
rtVar = RtStackVar

rtFree :: Natural -> RtCtl
rtFree = RtFreeVar 

data BuiltIns = BuiltIns
  { unit :: RtCtl,
    true :: RtCtl,
    false :: RtCtl
  }

builtIns :: BuiltIns
builtIns =
  let go =
        BuiltIns
          { unit = RtAllocProd $ Vec.fromList [],
            true = RtAllocCon 0 (unit go),
            false = RtAllocCon 1 (unit go)
          }
   in go

test1 :: IO ()
test1 = do
  let id = RtStackVar 0
  let const = RtStackVar 0
  fx <- newRef . rtBox $ id `rtApp` [rtInt 2]
  y <- newRef . rtBox $ rtInt 1
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef (rtBox const) (Vec.fromList [fx, y])

test2 :: IO ()
test2 = do
  let snd = RtEval (rtVar 0) (RtIndex (rtVar 0) 1)
  prd <- newRef . rtBox $ rtProd [rtInt 1, rtInt 2]
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef (rtBox snd) (Vec.fromList [prd])

test3 :: IO ()
test3 = do
  let flip = rtThunk [] $ 
        rtCase 0 $
          [ (MInt 0, RtAllocPrim $ RtInt 1),
            (MInt 1, RtAllocPrim $ RtInt 0)
          ]
  x <- newRef . rtBox $ rtInt 0
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef (rtBox flip) (Vec.fromList [x])

test4 :: IO ()
test4 = do
  let b = builtIns
  let isOne = rtThunk [] $ 
        rtCase 0 $
          [ (MInt 1, true b),
            (MAny, false b)
          ]
  x <- newRef . rtBox $ rtInt 0
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef (rtBox isOne) (Vec.fromList [x])

test5 :: IO ()
test5 = do
  let b = builtIns
  -- not = \x -> case x of
  --  True -> False
  --  False -> True
  let not = rtThunk [] $
        rtCase 0 $
          [ (MCon 0, false b),
            (MCon 1, true b)
          ]
  let code = not `rtApp` [true b]
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef (rtBox code) Vec.empty

test6 :: IO ()
test6 = do
  -- const = \x y -> x
  let const = rtThunk [] $ rtVar 0
  -- flip = \f y x -> f x y
  let flip = rtThunk [] $ rtVar 0 `rtApp` [rtVar 2, rtVar 1]
  -- code = (\f y x -> f x y) (\x y -> x) 2 1
  let code = flip `rtApp` [const, rtInt 2 , rtInt 1]
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef (rtBox code) Vec.empty

test7 :: IO ()
test7 = do
  -- const = \x y -> x
  let const = rtThunk [] $ rtVar 0
  -- func = \y f x -> f x y
  let func = rtThunk [] $ rtVar 1 `rtApp` [rtVar 2, rtVar 0]
  -- code = (\y f x -> f x y) 2 (\x y -> x) 1
  let code = func `rtApp` [rtInt 2, const, rtInt 1]
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef (rtBox code) Vec.empty

-- | Access to free variables works
test8 :: IO ()
test8 = do
  -- code = func 2 const 1
  let code = rtFree 0 `rtApp` [rtInt 2, rtFree 1, rtInt 1]
  -- func = \y f x -> f x y
  func <- newRef $ RtThunk (rtVar 1 `rtApp` [rtVar 2, rtVar 0]) Vec.empty
  -- const = \x y -> x
  const <- newRef $ RtThunk (rtVar 0) Vec.empty
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef (RtThunk code (Vec.fromList [func, const])) Vec.empty