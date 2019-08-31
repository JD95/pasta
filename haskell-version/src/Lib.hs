{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}

module Lib
  ( someFunc
  )
where

import qualified Data.Map.Strict               as Map
import           Data.Functor.Const
import           Data.Functor.Foldable
import           Data.Functor.Identity
import           Data.Void
import           Numeric.Natural

import           Expr
import           Typed
import           Surface
import           Core

test = do
  let exp = app (lam "x" (lam "y" (free "x"))) (free "thing")
  putStrLn . cata printSurface $ exp
  putStrLn . cata printCore . toCore $ exp
  putStrLn
    . cata printCore
    . flip
        eval
        (Map.fromList [("thing", (free "thing")), ("dude", free "dude")], [])
    . toCore
    $ exp

someFunc :: IO ()
someFunc = putStrLn "hello"
