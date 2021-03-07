{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Runtime () where

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
  | -- | Eval a closure,
    RtEval RtCtl RtCtl
  | -- | Index into a product
    RtIndex RtCtl Natural
  | -- |
    RtApp RtCtl (Vector RtCtl)
  | -- | Allocate a new closure
    RtAllocThunk RtCtl (Vector RtCtl)
  | -- |
    RtAllocProd (Vector RtCtl)
  | -- |
    RtAllocCon Natural RtCtl
  | -- |
    RtAllocPrim PrimVal
  | -- | Reference to a variable on the stack frame
    RtStackVar Natural
  | -- | Reference to a free variable
    RtFreeVar Natural
    deriving Show

parens :: String -> String
parens x = "(" <> x <> ")"

rtDisplayCtl :: RtCtl -> String
rtDisplayCtl x = go x where
  go (RtApp f xs) = "(" <> go f <> ") " <> intercalate " " (Vec.toList $ parens . go <$> xs)
  go (RtStackVar i) = "#" <> show i
  go (RtAllocPrim x) = show x
  go (RtAllocThunk f frees) = concat ["(", go f, ") {", intercalate " " (Vec.toList $ parens . go <$> frees), "}"] 

data RtClo r
  = -- | An unevaluated thunk, result of application
    RtThunk
      -- | Eval Code
      RtCtl
      -- | Captured Free Variables
      (Vector (r (RtClo r)))
  | -- | An evaluated thunk
    RtWhnf (RtVal r)

data PrimVal
  = RtInt Int
    deriving Show

data RtVal r
  = RtPrim PrimVal
  | RtProd (Vector (r (RtClo r)))
  | RtCon Natural (r (RtClo r))

rtDisplay :: RtClo r -> String
rtDisplay (RtWhnf (RtPrim (RtInt i))) = show i
rtDisplay (RtWhnf (RtCon tag _)) = "Con " <> show tag <> " _"
rtDisplay (RtThunk _ _) = "Thunk"

rtDisplay' :: (Log m, Ref m r) => RtClo r -> m String
rtDisplay' (RtWhnf (RtPrim (RtInt i))) = pure $ show i
rtDisplay' (RtWhnf (RtCon tag x)) = do
  x' <- rtDisplay' =<< readRef x
  pure $ concat ["Con ", show tag, " ", x']
rtDisplay' thunk@(RtThunk _ _) = rtDisplay' =<< rtEval thunk (Vec.empty)

class Monad m => Ref m r where
  newRef :: a -> m (r a)
  readRef :: r a -> m a
  writeRef :: r a -> a -> m ()

instance Ref IO IORef where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

rtMatch :: RtVal r -> Match -> Bool
rtMatch _ MAny = True
rtMatch (RtPrim (RtInt i)) (MInt j) = i == j
rtMatch (RtCon tag _) (MCon i) = tag == i

check :: RtVal r -> [(Match, RtCtl)] -> RtCtl
check switch xs = case filter (rtMatch switch . fst) xs of
  [] -> error "Non-exhaustive pattern match"
  ((_, p) : _) -> p

data RtEnv r = RtEnv {rtStackFrame :: Vector (r (RtClo r)), rtFrees :: Vector (r (RtClo r))}

rtDisplayEnv :: Ref m r => RtEnv r -> m String
rtDisplayEnv env = do
  let dis = fmap (intercalate " " . Vec.toList) . traverse (fmap (parens . rtDisplay) . readRef)
  frame <- dis $ rtStackFrame env
  frees <- dis $ rtFrees env
  pure $ concat [replicate 8 '-', "\nStack Frame: {" <>  frame, "}\nFree Vars: {", frees, "}"]
  
class Monad m => Log m where
  log :: String -> m ()

instance Log IO where
  log = putStrLn

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
    go env (RtIndex x i) = do
      go env x >>= readRef >>= \case
        (RtWhnf (RtProd xs)) -> pure $ xs Vec.! fromIntegral i
        (RtWhnf _) -> error "Can only index a product!"
        (RtThunk _ _) -> error "Cannot index a thunk!"
rtEval r _ = pure r

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

rtCase :: Natural -> [(Match, RtCtl)] -> RtCtl
rtCase x = RtEval (RtStackVar x) . RtBranch (RtStackVar x) . Vec.fromList

rtInt :: Int -> RtCtl
rtInt = RtAllocPrim . RtInt

rtProd :: [r (RtClo r)] -> RtClo r
rtProd = RtWhnf . RtProd . Vec.fromList

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
  x <- newRef . rtBox $ rtInt 1
  y <- newRef . rtBox $ rtInt 2
  prd <- newRef $ rtProd [x, y]
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
  let not = rtThunk [] $
        rtCase 0 $
          [ (MCon 0, false b),
            (MCon 1, true b)
          ]
  let code = not `rtApp` [true b]
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef (rtBox code) Vec.empty

test6 :: IO ()
test6 = do
  -- flip = \f y x -> f x y
  let const = rtThunk [] $ rtVar 0
  let flip = rtThunk [] $ rtVar 0 `rtApp` [rtVar 2, rtVar 1]
  let code = flip `rtApp` [const, rtInt 2 , rtInt 1]
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef (rtBox code) Vec.empty

test7 :: IO ()
test7 = do
  let const = rtThunk [] $ rtVar 0
  let func = rtThunk [] $ rtVar 1 `rtApp` [rtVar 2, rtVar 0]
  let code = func `rtApp` [rtInt 2, const, rtInt 1]
  putStrLn "Should be 1"
  putStrLn =<< rtDisplay' =<< rtEval @IO @IORef (rtBox code) Vec.empty
