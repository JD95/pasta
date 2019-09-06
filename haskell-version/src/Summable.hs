{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Summable where

import           Data.Void

class Summable (fs :: [* -> *]) where
  data Summed fs :: * -> *

instance Summable '[] where
  data Summed '[] a = SummedNil Void deriving Functor

instance Summable (f ': fs) where
  data Summed (f ': fs) a = Here (f a) | There (Summed fs a)

deriving instance (Functor f, Functor (Summed fs)) => Functor (Summed (f ': fs))

class Injectable (f :: * -> *) (fs :: [* -> *]) where
  inj :: f a -> Summed fs a

instance Functor f => Injectable f (f ': fs) where
  inj = Here

instance {-# OVERLAPPABLE #-} Injectable f fs => Injectable f (g ': fs) where
  inj = There . inj

class Outjectable (f :: * -> *) (fs :: [* -> *]) where
  outj :: Summed fs a -> Maybe (f a)

instance Outjectable f (f ': fs) where
    outj (Here fa)     = Just fa
    outj (There _) = Nothing

instance {-# OVERLAPPABLE #-} Outjectable f fs => Outjectable f (g ': fs) where
    outj (Here _ )      = Nothing
    outj (There fa) = outj fa

class ( Summable fs
      , Injectable f fs
      , Outjectable f fs
      , Functor (Summed fs)
      ) => (f :: * -> *) :<: (fs :: [* -> *])

instance ( Summable fs
         , Injectable f fs
         , Outjectable f fs
         , Functor (Summed fs)
         ) => (f :<: fs)
