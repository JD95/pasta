{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Eval.Normal where

import AST.Core
import AST.Transform
import Control.Monad (forM)
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Data.Functor.Const
import Data.Functor.Foldable (Fix (..), cata)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Sum
import Eval.Stages
import Prelude hiding (lookup)

-- -----------------------------------------------------------

-- |
-- * Evaluates WHNF terms into NF, removing
--   all closures.
class Normal f where
  normal ::
    Members '[Reader [Fix NF], IO] es =>
    f (Eff es (Fix NF)) ->
    Eff es (Fix NF)

instance Apply Normal fs => Normal (Sum fs) where
  normal = apply @Normal normal

instance Normal Term where
  normal = normal . unTerm

instance Normal (Ref (Fix Term)) where
  normal (Ref r) = do
    val <- send $ readIORef r
    result <- cata normal val
    send $ writeIORef r (Fix . Term . inject . Const $ result)
    r' <- send $ newIORef result
    pure . nf . ref $ r'

instance Normal Prim where
  normal = gpass NF

instance Normal Data where
  normal = gpass NF

instance Normal (Thunk (Fix Term)) where
  normal (Thunk st a) = do
    st' <- forM st $ cata normal
    local (const st') a

instance Normal Bound where
  normal (Bound i) = do
    st <- ask @[Fix NF]
    pure $ st !! fromIntegral i

instance Normal Norm where
  normal = pure . getConst

runNormal :: [Fix NF] -> Fix Term -> IO (Fix NF)
runNormal st = runM . runReader st . cata normal
