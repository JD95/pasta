{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module AST.Core.Prim where

import Data.Functor.Classes (Eq1 (..), Show1 (..))
import Data.Text (Text, pack, unpack)
import Display
import Numeric.Natural (Natural)

data Prim a
  = Arr a a
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

deriving instance Functor Prim

deriving instance Foldable Prim

deriving instance Traversable Prim

deriving instance (Eq a) => Eq (Prim a)

instance Eq1 Prim where
  liftEq _ (PInt x) (PInt y) = x == y
  liftEq _ _ _ = undefined

instance Show1 Prim where
  liftShowsPrec f _ _ p = unpack . displayF . fmap pack . traverse (f 0) p

instance Display Prim where
  displayF (Arr i o) = i <> " -> " <> o
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

data Polarity
  = -- | Evaluate expression to normal form
    Deep
  | -- | Evaluate expression to WHNF
    Shallow
  | -- | Suspend expression
    Lazy
  deriving (Eq, Ord)

data Usage
  = Zero
  | Once
  | Many
  deriving (Eq, Ord)
