{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module AST.Core where

import           Data.Foldable
import           Data.Functor.Foldable (Fix(..))
import           Data.List hiding (concat)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sum
import           Data.Text (Text, pack)
import qualified Data.Text as Text
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Numeric.Natural (Natural)

import           Display

data Prim a
  = Arr a a
  | NewTy !Text a
  | Type !Natural
  | Pole !Polarity
  | Rig !Usage
  --
  | PInt !Int
  | IntTy
  --
  | PNat !Natural
  | NatTy
  --
  | PFloat !Float
  | FloatTy
  --
  | PDouble !Double
  | DoubleTy
  --
  | PChar !Char
  | CharTy
deriving instance Functor Prim 
deriving instance Foldable Prim 
deriving instance Traversable Prim 

instance Display Prim where
  displayF (Arr i o) = i <> " -> " <> o
  displayF (NewTy n _) = n 
  displayF (Type _) = "Type" 
  displayF (Pole Deep) = "!" 
  displayF (Pole Shallow) = "+" 
  displayF (Pole Lazy) = "~" 
  displayF (Pole Logic) = "?" 
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

(-:>) :: (Prim :< fs) => Fix (Sum fs) -> Fix (Sum fs) -> Fix (Sum fs)
i -:> o = Fix . inject $ Arr i o

new_ :: (Prim :< fs) => Text -> Fix (Sum fs) -> Fix (Sum fs)
new_ name = Fix . inject . NewTy name

ty :: (Prim :< fs) => Natural -> Fix (Sum fs)
ty = Fix . inject . Type

data Polarity
  = Deep
  -- ^ Evaluate expression to normal form
  | Shallow
  -- ^ Evaluate expression to WHNF
  | Lazy
  -- ^ Suspend expression
  | Logic
  -- ^ Suspend with unresolved logic symbols 

data Usage 
  = Zero
  | Once
  | Many

-- | Lambda Terms
data Lam a = Lam Text a 
deriving instance Functor Lam

instance Display Lam where
  displayF (Lam input body) = "\\" <> input <> " -> " <> body

lam :: (Lam :< fs) => Text -> Fix (Sum fs) -> Fix (Sum fs)
lam input = Fix . inject . Lam input

-- | Application terms
data App a = App a a
deriving instance Functor App

app :: (App :< fs) => Fix (Sum fs) -> Fix (Sum fs) -> Fix (Sum fs)
app func = Fix . inject . App func 

instance Display App where
  displayF (App func input) = "(" <> func <> " " <> input <> ")"

data Data a where
  -- | A grouping of values
  Struct :: Vector a -> Data a

  -- | Indexing into a struct
  Out :: Natural -> a -> Data a

  -- | A tag for union types
  In :: Natural -> a -> Data a

  -- | Branching on a value
  Case :: a -> Map Natural a -> Data a
deriving instance Functor Data
deriving instance Foldable Data
deriving instance Traversable Data

struct :: (Data :< fs) => [Fix (Sum fs)] -> Fix (Sum fs)
struct = Fix . inject . Struct . V.fromList

case_ :: (Data :< fs) => Fix (Sum fs) -> [(Natural, Fix (Sum fs))] -> Fix (Sum fs)
case_ sub = Fix . inject . Case sub . Map.fromList

in_ :: (Data :< fs) => Natural -> Fix (Sum fs) -> Fix (Sum fs)
in_ i = Fix . inject . In i

out_ :: (Data :< fs) => Natural -> Fix (Sum fs) -> Fix (Sum fs)
out_ i = Fix . inject . Out i

instance Display Data where
  displayF (Struct v) = "(" <> (Text.concat . intersperse ", " . toList $ v) <> ")"
  displayF (Case sub paths) = "case " <> sub <> " of { " <> cases <> "}" where
    cases = Text.concat . intersperse "; " . fmap displayPair . Map.assocs $ paths
    displayPair (key, path) = (pack . show $ key) <> " -> " <> path
  displayF (Out i xs) = xs <> "@" <> (pack . show $ i)
  displayF (In i xs) = xs <> "#" <> (pack . show $ i)

newtype FreeVar a = FreeVar Text
deriving instance Functor FreeVar

free :: (FreeVar :< fs) => Text -> Fix (Sum fs)
free var = Fix . inject $ FreeVar var

instance Display FreeVar where
  displayF (FreeVar v) = v

type Core = Sum [Prim, Data, Lam, App, FreeVar]
