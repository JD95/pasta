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

import Data.Functor.Foldable (Fix (..))
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
