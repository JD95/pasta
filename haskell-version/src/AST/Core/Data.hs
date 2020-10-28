{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AST.Core.Data where

import AST.Transform
import Control.Monad.Free
import Data.Eq.Deriving
import Data.Foldable
import Data.Functor.Classes (Eq1 (..), Show1 (..))
import Data.Functor.Foldable (Fix (..))
import Data.List hiding (concat)
import Data.Sum
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import Display
import Numeric.Natural (Natural)
import RIO hiding (Data)
import RIO.Map (Map)
import qualified RIO.Map as Map
import Text.Show.Deriving

data Data a where
  -- | A grouping of values
  Struct :: Vector a -> Data a
  -- | Indexing into a struct
  Out :: Natural -> a -> Data a
  -- | A tag for union types
  In :: Natural -> a -> Data a
  -- | Branching on a value
  Case :: a -> Map Natural a -> Data a

deriving instance (Eq a) => Eq (Data a)

deriving instance Functor Data

deriving instance Foldable Data

deriving instance Traversable Data

deriveEq1 ''Data
deriveShow1 ''Data

instance Diffable Data where
  diff f (Struct _) (Struct _) = undefined
  diff f (Out _ _) (Out _ _) = undefined
  diff f (In _ _) (In _ _) = undefined
  diff f (Case _ _) (Case _ _) = undefined
  diff _ _ _ = undefined

struct :: (Foldable t, Data :< fs) => t (Free (Sum fs) a) -> Free (Sum fs) a
struct = Free . inject . Struct . V.fromList . toList

case_ :: (Data :< fs) => Free (Sum fs) a -> [(Natural, Free (Sum fs) a)] -> Free (Sum fs) a
case_ sub = Free . inject . Case sub . Map.fromList

in_ :: (Data :< fs) => Natural -> Free (Sum fs) a -> Free (Sum fs) a
in_ i = Free . inject . In i

out_ :: (Data :< fs) => Natural -> Free (Sum fs) a -> Free (Sum fs) a
out_ i = Free . inject . Out i

instance DisplayF Data where
  displayF (Struct v) = "(" <> (Text.concat . intersperse ", " . toList $ v) <> ")"
  displayF (Case sub paths) = "case " <> sub <> " of { " <> cases <> "}"
    where
      cases = Text.concat . intersperse "; " . fmap displayPair . Map.assocs $ paths
      displayPair (key, path) = (pack . show $ key) <> " -> " <> path
  displayF (Out i xs) = xs <> "@" <> (pack . show $ i)
  displayF (In i xs) = xs <> "#" <> (pack . show $ i)
