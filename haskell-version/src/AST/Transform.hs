{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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
import Logic.Info

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

-- | Represents the result of diffing two
-- structures.
data Diff a
  = -- | The structure was unchanged
    Same a
  | -- | Changes between versions could be reconciled
    Update a
  | -- | Changes between versions could not be reconciled
    Conflict
  deriving (Eq, Show, Functor)

instance Applicative Diff where
  pure = Same

  Update f <*> Update x = Update (f x)
  Update f <*> Same x = Update (f x)
  Same f <*> Update x = Update (f x)
  Same f <*> Same x = Same (f x)
  _ <*> _ = Conflict

class Diffable f where
  diff :: (a -> a -> Diff a) -> f a -> f a -> Diff (f a)

instance Apply Diffable fs => Diffable (Sum fs) where
  diff f x y = maybe Conflict id $ apply2' @Diffable (\inj a b -> inj <$> diff f a b) x y

diffEq :: Eq a => a -> a -> Diff a
diffEq x y
  | x == y = Same x
  | otherwise = Conflict

diffToInfo :: Diff a -> Info a
diffToInfo (Update x) = Info x
diffToInfo (Same _) = NoInfo
diffToInfo _ = Contradiction
