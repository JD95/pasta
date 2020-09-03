{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Logic.Datalog where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import qualified Data.HashSet as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Logic.Info
import Logic.Monad
import Logic.Propagator
import Logic.Propagator.PrimCell

data Edge = Edge {edgeFrom :: String, edgeTo :: String} deriving (Show, Eq)

data Table a = Table (Vector a)

edge :: (PrimMonad m, Network m, Cell m f, Cell m g) => f String -> g String -> Table Edge -> m ()
edge in1 in2 (Table tbl) = do
  tbl' <- newPrimCell
  inform (Info tbl) tbl'

  waitOn [SomeCell in1] $
    flip propagate [tbl'] $ do
      content in1 >>= \case
        Info x ->
          content tbl' >>= \case
            Info rows -> pure . Info $ Vec.filter ((==) x . edgeFrom) rows
            _ -> pure NoInfo
        _ -> pure NoInfo

  waitOn [SomeCell in2] $
    flip propagate [tbl'] $ do
      content in2 >>= \case
        Info y ->
          content tbl' >>= \case
            Info rows -> pure . Info $ Vec.filter ((==) y . edgeTo) rows
            _ -> pure NoInfo
        _ -> pure NoInfo

  waitOn [SomeCell tbl'] $
    flip propagate [in1] $ do
      content tbl' >>= \case
        Info vals -> msum (map (pure . Info) . Set.toList . Vec.foldr Set.insert Set.empty . Vec.map edgeFrom $ vals)
        _ -> pure NoInfo

  waitOn [SomeCell tbl'] $
    flip propagate [in2] $ do
      content tbl' >>= \case
        -- Simply splitting universes on every appropriate value here
        -- leads to duplicate answers. This is where subsumption tables
        -- would come into play, but to compensate for that not being
        -- implemented yet, simply split on the *unique* values
        -- in the row.
        Info vals ->
          msum
            ( map (pure . Info)
                -- Only care about unique  values
                . Set.toList
                . Vec.foldr Set.insert Set.empty
                -- Grab appropriate row
                . Vec.map edgeTo
                $ vals
            )
        _ -> pure NoInfo

path :: (PrimMonad m, Network m, Cell m f, Cell m g) => f String -> g String -> Table Edge -> m ()
path x y tbl = case1 <|> case2
  where
    case1 = edge x y tbl

    case2 = do
      z <- newPrimCell
      edge x z tbl
      waitOn [SomeCell z] $ path z y tbl

test :: IO [Info String]
test = observeAllT $ do
  x <- newPrimCell
  y <- newPrimCell
  path x y tbl
  inform (Info "A") x
  content y
  where
    tbl = Table $ Vec.fromList [Edge "A" "B", Edge "B" "C", Edge "A" "D"]
