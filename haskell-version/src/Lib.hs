{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
  ( someFunc
  )
where

import           Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import           Data.Functor.Foldable

import           Core
import           Core.Eval
import           Core.EvalEnv
import           Expr
import           Surface
import           Typed

test :: IO ()
test = do
  -- let exp = app (lam "x" (lam "y" (free "x"))) (free "thing")
  let e = arrow ("a", Inline R0, Inline S, Inline S) (con "Type")
             (arrow ("x", Inline R0, Inline S, Inline S) (free "a") (free "a"))
  let ctx = Map.fromList [("thing", (free "thing")), ("dude", free "dude")]
  putStrLn . cata printSurface $ e
  putStrLn . cata printCore . toCore $ e
  putStrLn
    . cata printCore
    . flip evalState ctx
    . runEvalEnv
    . eval
    . toCore
    $ e

someFunc :: IO ()
someFunc = putStrLn "hello"
