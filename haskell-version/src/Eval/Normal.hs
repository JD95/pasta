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
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.ST (ST)
import Data.Functor.Const
import Data.Functor.Foldable (Fix (..), cata, unfix)
import Data.IORef (IORef, readIORef)
import Data.Sum
import Eval.Stages
import Prelude hiding (lookup)

-- -----------------------------------------------------------

-- |
-- * Evaluates WHNF terms into NF, removing
--   all closures.
class Normal f where
  normal ::
    Members '[Reader (Stack (Fix Term)), IO] es =>
    f (Eff es (Fix NF)) ->
    Eff es (Fix NF)

instance Apply Normal fs => Normal (Sum fs) where
  normal = apply @Normal normal

instance Normal Term where
  normal = normal . unTerm

instance Normal Prim where
  normal = pass

instance Normal Data where
  normal = pass

instance Normal (Thunk (Fix Term)) where
  normal (Thunk st a) = local (const st) a

instance Normal Bound where
  normal (Bound i) = do
    (Stack st) <- ask @(Stack (Fix Term))
    let ref = st !! fromIntegral i
    val <- send $ readIORef ref
    cata normal val

instance Normal Norm where
  normal = pure . getConst

runNormal :: Stack (Fix Term) -> Fix Term -> IO (Fix NF)
runNormal st = runM . runReader st . cata normal
