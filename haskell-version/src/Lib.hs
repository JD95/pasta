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

import qualified Data.Map.Strict               as Map
import           Data.Functor.Foldable
import           Data.Proxy
import           Polysemy
import           Polysemy.State

import           Core
import           Core.Eval
import           Core.EvalEnv
import           Expr
import           Surface
import           Typed
import           Display

test :: IO ()
test = do
  let
    (e :: Fix SurfaceE) = mkArrow
      se
      ("a", Inline R0, Inline S, Inline S)
      (mkCon se "Type")
      (mkArrow se
               ("x", Inline R0, Inline S, Inline S)
               (mkFree se "a")
               (mkFree se "a")
      )
  let
    ctx =
      Map.fromList [("thing", (mkFree ce "thing")), ("dude", mkFree ce "dude")]
  putStrLn . cata display $ e
  putStrLn . cata display . toCore $ e
  putStrLn
    . cata display
    . run
    . evalState @(Map.Map String (Fix CoreE)) ctx
    . runSymLookupState @String @(Fix CoreE)
    . eval
    . toCore
    $ e

someFunc :: IO ()
someFunc = putStrLn "hello"
