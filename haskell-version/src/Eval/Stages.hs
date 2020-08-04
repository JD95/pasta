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

module Eval.Stages where

import AST.Core
import Data.Functor.Classes
import Data.Functor.Const
import Data.Functor.Foldable (Fix (..), unfix)
import Data.IORef (IORef, newIORef)
import Data.Sum
import Data.Text (pack, unpack)
import Display
import Numeric.Natural

-- | The runtime stack, holding pointers to closures
data Thunk b a = Thunk [b] a

deriving instance Functor (Thunk b)

thunk :: (Thunk b :< fs) => [b] -> Fix (Sum fs) -> Fix (Sum fs)
thunk st = Fix . inject . Thunk st

newtype Bound a = Bound Natural

deriving instance Functor Bound

bnd :: (Bound :< fs) => Natural -> Fix (Sum fs)
bnd = Fix . inject . Bound

newtype Ref b a = Ref (IORef b)

deriving instance Functor (Ref b)

deriving instance Foldable (Ref b)

deriving instance Traversable (Ref b)

deriving instance Eq (Ref b a)

instance Eq1 (Ref b) where
  liftEq _ (Ref a) (Ref b) = a == b

instance Display (Ref (Fix NF)) where
  displayF _ = "#ref"

ref :: (Ref b :< fs) => IORef b -> Fix (Sum fs)
ref = Fix . inject . Ref

type TermComps b = Sum [Prim, Data, Thunk b, Ref b, Bound, Norm]

-- | Expressions during and after eval
newtype Term a = Term {unTerm :: TermComps (Fix Term) a}

term :: (fs ~ TermComps (Fix Term)) => Fix fs -> Fix Term
term = Fix . Term . fmap term . unfix

alloc :: (fs ~ TermComps (Fix Term)) => Fix fs -> IO (Fix Term)
alloc = fmap (term . ref) . newIORef . term

deriving instance Functor Term

type Norm = Const (Fix NF)

type NfComps b = Sum '[Prim, Data, Ref b]

-- | Fully evaluated term
newtype NF a = NF {unNF :: NfComps (Fix NF) a}

deriving instance Functor NF

deriving instance Foldable NF

deriving instance Traversable NF

instance Display NF where
  displayF (NF val) = displayF val

instance Eq1 NF where
  liftEq f (NF a) (NF b) = liftEq f a b

instance Show1 NF where
  liftShowsPrec f _ _ p = unpack . displayF . fmap pack . traverse (f 0) p

nf :: (fs ~ NfComps (Fix NF)) => Fix fs -> Fix NF
nf = Fix . NF . fmap nf . unfix
