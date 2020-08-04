{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module AST.Core.Data where

import Data.Foldable
import Data.Functor.Classes (Eq1 (..), Show1 (..))
import Data.Functor.Foldable (Fix (..))
import Data.List hiding (concat)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sum
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import Display
import Numeric.Natural (Natural)

data Data a where
  -- | A grouping of values
  Struct :: Vector a -> Data a
  -- | Indexing into a struct
  Out :: Natural -> a -> Data a
  -- | A tag for union types
  In :: Natural -> a -> Data a
  -- | Branching on a value
  Case :: a -> Map Natural a -> Data a

instance Show1 Data where
  liftShowsPrec f _ _ p = unpack . displayF . fmap pack . traverse (f 0) p

deriving instance (Eq a) => Eq (Data a)

instance Eq1 Data where
  liftEq _ _ _ = undefined

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
  displayF (Case sub paths) = "case " <> sub <> " of { " <> cases <> "}"
    where
      cases = Text.concat . intersperse "; " . fmap displayPair . Map.assocs $ paths
      displayPair (key, path) = (pack . show $ key) <> " -> " <> path
  displayF (Out i xs) = xs <> "@" <> (pack . show $ i)
  displayF (In i xs) = xs <> "#" <> (pack . show $ i)
