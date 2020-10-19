{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module TypeCheck.Typed.Render where

import AST.Core
import AST.Transform
import Control.Monad.Free
import Control.Monad.Freer
import Control.Monad.Freer.State
import Data.Functor.Foldable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Text
import TypeCheck.Typed.Stages

data RenderST = RenderST {idMap :: IntMap Text, names :: [String]}

newRenderST :: RenderST
newRenderST = RenderST mempty ns
  where
    ns = Prelude.concat $ iterate (Prelude.zipWith (<>) start) start
      where
        start = (: []) <$> ['a' .. 'z']

lookupName :: Members '[State RenderST] es => Int -> Eff es (Maybe Text)
lookupName i = IntMap.lookup i . idMap <$> get

addId :: Members '[State RenderST] es => Int -> Text -> Eff es ()
addId i t = modify (\st -> st {idMap = IntMap.alter (const . Just $ t) i (idMap st)})

popNextName :: Members '[State RenderST] es => Eff es Text
popNextName = do
  name <- (pack . Prelude.head . names) <$> get
  modify (\st -> st {names = Prelude.tail $ names st})
  pure name

renderHoles :: Partial Hole -> Fix Typed
renderHoles = asFix . run . evalState newRenderST . traverse go
  where
    go :: Members '[State RenderST] es => Hole -> Eff es (Fix Typed)
    go h =
      lookupName (unHole h) >>= \case
        Just name -> pure . asFix $ free name
        Nothing -> do
          newName <- popNextName
          addId (unHole h) newName
          pure . asFix $ free newName
