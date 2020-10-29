{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Eval.Stages where

import AST.Core
import AST.Transform
import Data.Functor.Classes
import Data.Functor.Const
import Data.Functor.Foldable (Fix (..), unfix)
import Data.Sum
import Data.Text (pack, unpack)
import Display
import Numeric.Natural
import RIO hiding (Data)

-- | The runtime stack, holding pointers to closures
data Thunk b a = Thunk [b] a

deriving instance Functor (Thunk b)

deriving instance Foldable (Thunk b)

deriving instance Traversable (Thunk b)

class InjThunk b f where
  injThunk :: Thunk b a -> f a

instance Thunk b :< fs => InjThunk b (Sum fs) where
  injThunk = inject

thunk :: (AST f a, InjThunk b f) => [b] -> a -> a
thunk st = form . injThunk . Thunk st

newtype Bound a = Bound Natural

deriving instance Functor Bound

deriving instance Foldable Bound

deriving instance Traversable Bound

class InjBound f where
  injBound :: Bound a -> f a

instance Bound :< fs => InjBound (Sum fs) where
  injBound = inject

bnd :: (AST f a, InjBound f) => Natural -> a
bnd = form . injBound . Bound

newtype Ref b a = Ref (IORef b)

deriving instance Functor (Ref b)

deriving instance Foldable (Ref b)

deriving instance Traversable (Ref b)

deriving instance Eq (Ref b a)

instance Eq1 (Ref b) where
  liftEq _ (Ref a) (Ref b) = a == b

instance DisplayF (Ref (Fix NF)) where
  displayF _ = "#ref"

class InjRef b f where
  injRef :: Ref b a -> f a

instance Ref b :< fs => InjRef b (Sum fs) where
  injRef = inject

ref :: (AST f a, InjRef b f) => IORef b -> a
ref = form . injRef . Ref

type TermComps b = Sum [Prim, Data, Thunk b, Ref b, Bound, Norm]

-- | Expressions during and after eval
newtype Term a = Term {unTerm :: TermComps (Fix Term) a}

deriving instance Functor Term

deriving instance Foldable Term

deriving instance Traversable Term

instance InjPrim Term where
  injPrim = Term . inject

instance InjData Term where
  injData = Term . inject

instance InjThunk (Fix Term) Term where
  injThunk = Term . inject

instance InjRef (Fix Term) Term where
  injRef = Term . inject

instance InjBound Term where
  injBound = Term . inject

alloc :: Fix Term -> IO (Fix Term)
alloc = fmap ref . newIORef

type Norm = Const (Fix NF)

type NfComps b = Sum '[Prim, Data, Ref b]

-- | Fully evaluated term
newtype NF a = NF {unNF :: NfComps (Fix NF) a}

deriving instance Functor NF

deriving instance Foldable NF

deriving instance Traversable NF

instance DisplayF NF where
  displayF (NF val) = displayF val

instance Eq1 NF where
  liftEq f (NF a) (NF b) = liftEq f a b

instance Show1 NF where
  liftShowsPrec f _ _ p = unpack . displayF . fmap pack . traverse (f 0) p

instance InjPrim NF where
  injPrim = NF . inject

instance InjData NF where
  injData = NF . inject

instance InjRef (Fix NF) NF where
  injRef = NF . inject
