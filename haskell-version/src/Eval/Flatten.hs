{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Eval.Flatten where

import AST.Core
import AST.Transform
import Control.Monad (forM)
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Data.Functor.Const
import Data.Functor.Foldable (Fix (..), cata)
import Data.Sum
import Eval.Stages
import RIO hiding (Data)

type Flat = Sum '[Prim, Data]

class Flatten f where
  flatten :: f (IO (Fix Flat)) -> IO (Fix Flat)

instance Apply Flatten fs => Flatten (Sum fs) where
  flatten = apply @Flatten flatten

instance Flatten NF where
  flatten (NF x) = flatten x

instance Flatten (Ref (Fix NF)) where
  flatten (Ref r) = do
    val <- readIORef r
    cata flatten val

instance Flatten Prim where
  flatten = pass

instance Flatten Data where
  flatten = pass

runFlatten :: Fix NF -> IO (Fix Flat)
runFlatten = cata flatten
