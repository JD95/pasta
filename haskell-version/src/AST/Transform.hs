{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module AST.Transform where

import Control.Monad.Free
import Data.Functor.Foldable (Fix (..), cata)
import Data.Sum

-- | Allows for unchanging parts of the AST to pass through a transform
pass :: (f :< g, Monad m, Traversable f) => f (m (Fix (Sum g))) -> m (Fix (Sum g))
pass = gpass id

-- | A generalized 'pass' given a natural transform from `Sum g` to `h`
gpass ::
  (f :< g, Monad m, Traversable f) =>
  (forall x. Sum g x -> h x) ->
  f (m (Fix h)) ->
  m (Fix h)
gpass into = fmap (Fix . into . inject) . sequence

asFix :: Functor f => Free f (Fix f) -> Fix f
asFix = iter Fix

asFree :: Functor f => Fix f -> Free f a
asFree = cata Free
