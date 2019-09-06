{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Display where

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
