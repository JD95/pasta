{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Display where

import Data.List
import           Data.Functor.Foldable

import           Summable

class Display a where
  display :: a -> String

instance (Display (f a), Display (Summed fs a), Display a)
  => Display (Summed (f ': fs) a) where
  display (Here x) = display x
  display (There x) = display x

instance (Display a) => Display (Summed '[] a) where
  display _ = ""

instance Display String where
  display = id

displayF
  :: (Functor f, Functor g, Display (f String), Display (g String))
  => f (Fix g)
  -> String
displayF = display . fmap (cata display)

sepBy :: String -> [String] -> String
sepBy _ [] = ""
sepBy _ [x] = x
sepBy s xs = foldl' (\str (x,y) -> str <> x <> s <> y) "" (zip xs (tail xs))
