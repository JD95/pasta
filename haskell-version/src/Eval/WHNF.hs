{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Eval.WHNF where

import AST.Core
import AST.Transform
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Data.Functor.Foldable (Fix (..), cata)
import Data.IORef (readIORef, writeIORef)
import Data.Sum
import Eval.Stages
import RIO hiding (Data, Reader, ask, local, readIORef, runReader, writeIORef)
import RIO.List

class Whnf f where
  whnf ::
    Members '[Reader [Fix Term], IO] es =>
    f (Eff es (Fix Term)) ->
    Eff es (Fix Term)

instance Apply Whnf fs => Whnf (Sum fs) where
  whnf = apply @Whnf whnf

instance Whnf Term where
  whnf = whnf . unTerm

instance Whnf Prim where
  whnf = gpass Term

instance Whnf Data where
  whnf = gpass Term

instance Whnf (Ref (Fix Term)) where
  whnf (Ref r) = do
    val <- send $ readIORef r
    result <- cata whnf val
    send $ writeIORef r result
    pure $ ref r

instance Whnf (Thunk (Fix Term)) where
  whnf (Thunk st a) = local (const st) a

instance Whnf Bound where
  whnf (Bound i) = do
    st <- ask @[Fix Term]
    let val = case lastMaybe $ take (fromIntegral i + 1) st of
          Just x -> x
          Nothing -> undefined
    pure val

instance Whnf Norm where
  whnf = gpass Term

runWhnf :: [Fix Term] -> Fix Term -> IO (Fix Term)
runWhnf st = runM . runReader st . cata whnf
