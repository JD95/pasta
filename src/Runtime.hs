{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Runtime where

import Control.Monad
import Control.Monad.State
import Data.Functor.Foldable
import Data.Word
import Runtime.Types
import Prelude hiding (const, id, log)

data RtEnv = RtEnv {stack :: [RtVal]}

newtype EvalM a = EvalM {runEvalM :: State RtEnv a}
  deriving (Functor, Applicative, Monad, MonadState RtEnv)

push :: RtVal -> EvalM a -> EvalM a
push x action = do
  modify $ \st -> st {stack = x : stack st}
  result <- action
  modify $ \st -> st {stack = tail $ stack st}
  pure result

access :: Word32 -> EvalM RtVal
access i = (!! fromIntegral i) . stack <$> get

eval :: RtVal -> RtVal
eval val = evalState (runEvalM (cata go val)) (RtEnv [])
  where
    go :: RtValF (EvalM RtVal) -> EvalM RtVal
    go (RtProdF xs) = RtProd <$> sequence xs
    go (RtLamF evalBody) = evalBody
    go (RtAppF evalFunc evalInputs) = do
      inputs <- sequence evalInputs
      foldr push evalFunc inputs
    go (RtArrF evalInput evalOutput) = do
      input <- evalInput
      push input $ do
        output <- evalOutput
        pure $ RtArr input output
    go (RtVarF i) = access i
    go RtTyF = pure RtTy
    go (RtPrimF p) = pure $ RtPrim p
    go (RtConF i x) = RtCon i <$> x
