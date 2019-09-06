{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
  ( someFunc
  )
where

import           Control.Monad.State.Strict
import qualified Data.Map.Strict               as Map
import           Data.Functor.Foldable
import           Data.Proxy

import           Core
import           Core.Eval
import           Core.EvalEnv
import           Expr
import           Surface
import           Typed
import           Display

test :: IO ()
test = do
  -- let exp = app (lam "x" (lam "y" (free "x"))) (free "thing")
  let (ExprBuilder appS lamS varS freeS _) = exprBuilder (Proxy @Surface)
  let (TypeBuilder rig pol arrow con t)    = typeBuilder (Proxy @Surface)
  let (e :: Fix SurfaceE) = arrow
        ("a", Inline R0, Inline S, Inline S)
        (con "Type")
        (arrow ("x", Inline R0, Inline S, Inline S) (freeS "a") (freeS "a"))
  let (ExprBuilder _ _ _ free _) = exprBuilder (Proxy @Core)
  let ctx = Map.fromList [("thing", (free "thing")), ("dude", free "dude")]
  putStrLn . cata display $ e
  putStrLn . cata display . toCore $ e
  putStrLn . cata display . flip evalState ctx . runEvalEnv . eval . toCore $ e

someFunc :: IO ()
someFunc = putStrLn "hello"
