{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Runtime.Types where

{-
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List
import Data.List.NonEmpty
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Word
import Prelude hiding (const, id, log)

data Match
  = MInt Int
  | MCon Word32
  | MAny
  deriving (Show)

data PrimVal
  = RtInt Int
  deriving (Show, Eq)

data RtVal
  = RtPrim PrimVal
  | RtProd (Vector RtVal)
  | RtCon Word32 RtVal
  | RtLam RtVal
  | RtArr RtVal RtVal
  | RtApp RtVal (Vector RtVal)
  | RtVar Word32
  | RtDepTy Word32
  | RtTy
  | RtUnknown Word32
  | RtAmbiguous (NonEmpty RtVal)
  deriving (Show, Eq)

makeBaseFunctor ''RtVal

deriving instance Eq a => Eq (RtValF a)

unit :: RtVal
unit = RtProd Vec.empty

unitF :: RtValF a
unitF = RtProdF Vec.empty

displayRtVal :: RtVal -> String
displayRtVal = cata $ \case
  RtArrF input output -> parens $ input <> " -> " <> output
  RtAppF func ins -> intercalate " " $ func : Vec.toList ins
  RtProdF xs -> parens $ intercalate ", " (Vec.toList xs)
  RtVarF i -> "$" <> show i
  RtLamF body -> parens $ "\\ -> " <> body
  RtTyF -> "Type"
  RtDepTyF i -> "#" <> show i
  RtPrimF _ -> undefined
  RtUnknownF i -> "?" <> show i
  RtConF _ _ -> undefined
  RtAmbiguousF xs -> "{" <> intercalate ", " (toList xs) <> "}"
  where
    parens x = "(" <> x <> ")"
-}
