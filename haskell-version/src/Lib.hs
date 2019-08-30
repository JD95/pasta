{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}

module Lib
  ( someFunc
  )
where

import           Data.Functor.Const
import           Data.Functor.Foldable
import           Data.Functor.Identity
import           Data.Void
import           Numeric.Natural

import           Expr
import           Typed
import           Surface

test = do
  putStrLn . cata printSurface $ app (lam "x" (lam "y" (free "x")))
                                     (free "thing")

  putStrLn . cata printSurface $ rig "r" $ pol "p" (con "foo")
  putStrLn . cata printSurface $ rig "r" $ pol "p" $ arrow
    ("a", Inline R0, Inline S, Inline S)
    (con "Type")
    (arrow ("x", Free "r", Free "p", Free "p") (free "a") (free "a"))

someFunc :: IO ()
someFunc = putStrLn "someFunc"
