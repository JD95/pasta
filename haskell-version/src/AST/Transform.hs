{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module AST.Transform where

import           Data.Functor.Foldable (Fix(..))
import           Data.Sum

-- | Allows for unchanging parts of the AST to pass through a transform
pass :: (f :< g, Monad m, Traversable f) => f (m (Fix (Sum g))) -> m (Fix (Sum g))
pass = fmap (Fix . inject) . sequence 
