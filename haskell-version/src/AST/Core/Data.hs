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
{-# LANGUAGE UndecidableInstances #-}
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
  Out :: !Natural -> a -> Data a
  -- | A tag for union types
  In :: !Natural -> a -> Data a
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

class InjData f where
  injData :: Data a -> f a

instance Data :< f => InjData (Sum f) where
  injData = inject

struct :: (Foldable t, InjData f) => t (Free f a) -> Free f a
struct = Free . injData . Struct . V.fromList . toList

case_ :: (InjData f) => Free f a -> [(Natural, Free f a)] -> Free f a
case_ sub = Free . injData . Case sub . Map.fromList

in_ :: (InjData f) => Natural -> Free f a -> Free f a
in_ i = Free . injData . In i

out_ :: (InjData f) => Natural -> Free f a -> Free f a
out_ i = Free . injData . Out i

instance DisplayF Data where
  displayF (Struct v) = "(" <> (Text.concat . intersperse ", " . toList $ v) <> ")"
  displayF (Case sub paths) = "case " <> sub <> " of { " <> cases <> "}"
    where
      cases = Text.concat . intersperse "; " . fmap displayPair . Map.assocs $ paths
      displayPair (key, path) = (pack . show $ key) <> " -> " <> path
  displayF (Out i xs) = xs <> "@" <> (pack . show $ i)
  displayF (In i xs) = xs <> "#" <> (pack . show $ i)
