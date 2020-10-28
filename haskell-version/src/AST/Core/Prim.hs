{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AST.Core.Prim where

import AST.Transform
import Control.Applicative
import Control.Monad.Free
import Data.Eq.Deriving
import Data.Functor.Classes (Eq1 (..), Show1 (..))
import Data.Sum
import Data.Text (Text, pack, unpack)
import Display
import Logic.Info
import Numeric.Natural (Natural)
import RIO
import Text.Show.Deriving

data Polarity
  = -- | Evaluate expression to normal form
    Deep
  | -- | Evaluate expression to WHNF
    Shallow
  | -- | Suspend expression
    Lazy
  deriving (Eq, Ord, Show)

data Usage
  = Zero
  | Once
  | Many
  deriving (Eq, Ord, Show)

data Prim a
  = Arr !(Maybe Text) a a
  | NewTy !Text a
  | Type !Natural
  | Pole !Polarity
  | Rig !Usage
  | --
    PInt !Int
  | IntTy
  | --
    PNat !Natural
  | NatTy
  | --
    PFloat !Float
  | FloatTy
  | --
    PDouble !Double
  | DoubleTy
  | --
    PChar !Char
  | CharTy

deriving instance Show a => Show (Prim a)

deriving instance Functor Prim

deriving instance Foldable Prim

deriving instance Traversable Prim

deriving instance (Eq a) => Eq (Prim a)

deriveEq1 ''Prim
deriveShow1 ''Prim

instance Diffable Prim where
  diff f (Arr s a b) (Arr t c d) = Arr (s <|> t) <$> f a c <*> f b d
  diff f (NewTy s x) (NewTy t y) = s `diffEq` t *> (NewTy s <$> f x y)
  diff _ (Type n) (Type m) = Type <$> n `diffEq` m
  diff _ (PInt n) (PInt m) = PInt <$> n `diffEq` m
  diff _ IntTy IntTy = Same IntTy
  diff _ NatTy NatTy = Same NatTy
  diff _ x y = Conflict

instance DisplayF Prim where
  displayF (Arr (Just s) i o) = "(" <> s <> " : " <> i <> ") -> " <> o
  displayF (Arr Nothing i o) = i <> " -> " <> o
  displayF (NewTy n _) = n
  displayF (Type _) = "Type"
  displayF (Pole Deep) = "!"
  displayF (Pole Shallow) = "+"
  displayF (Pole Lazy) = "~"
  displayF (Rig Zero) = "0"
  displayF (Rig Once) = "1"
  displayF (Rig Many) = "w"
  displayF (PInt i) = pack . show $ i
  displayF (IntTy) = "Int"
  displayF (PNat n) = pack . show $ n
  displayF (NatTy) = "Nat"
  displayF (PFloat f) = pack . show $ f
  displayF (FloatTy) = "Float"
  displayF (PDouble d) = pack . show $ d
  displayF (DoubleTy) = "Double"
  displayF (PChar c) = pack . show $ c
  displayF (CharTy) = "Char"

class InjPrim f where
  injPrim :: Prim a -> f a

instance Prim :< f => InjPrim (Sum f) where
  injPrim = inject

(-:>) :: (InjPrim f) => Free f a -> Free f a -> Free f a
i -:> o = Free . injPrim $ Arr Nothing i o

infixr 2 -:>

pi :: InjPrim f => Text -> Free f a -> Free f a -> Free f a
pi s i o = Free . injPrim $ Arr (Just s) i o

new_ :: (InjPrim f) => Text -> Free f a -> Free f a
new_ name = Free . injPrim . NewTy name

ty :: (InjPrim f) => Natural -> Free f a
ty = Free . injPrim . Type

int :: (InjPrim f) => Int -> Free f a
int = Free . injPrim . PInt

intTy :: (InjPrim f) => Free f a
intTy = Free $ injPrim IntTy

natTy :: (InjPrim f) => Free f a
natTy = Free $ injPrim NatTy
