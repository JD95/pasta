{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception (SomeException (..), try)
import Control.Monad
import Data.Functor.Foldable (cata)
import Data.IORef (IORef)
import qualified Data.Vector as Vec
import Lib
import Runtime
import Runtime.Dsl
import Runtime.Ref
import Runtime.Types
import System.Environment
import TablingTests (tablingTests)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Options

main :: IO ()
main = withArgs ["--hide-successes"] $ defaultMain $ tests
  where
    tests = testGroup "Jelly" [runtime, tablingTests]

    runtime = testGroup "runtime" [informTests, propagationTests]

    informTests =
      testGroup
        "inform"
        [ testInformMergesValues,
          testInformFillsValue
        ]

    propagationTests =
      testGroup
        "propagation"
        [ testInformFillsValue,
          testInformMergesValues,
          testAddingCellDep,
          testTriggerProp
        ]

testInformMergesValues = testCase "inform will merge values" $ do
  -- Setup the cell
  let xCell = rtCell identityLat
  x <- join $ rtEval @IO @IORef <$> newRef (rtBox xCell) <*> pure mempty
  let yCell = rtCell identityLat
  y <- join $ rtEval @IO @IORef <$> newRef (rtBox yCell) <*> pure mempty
  -- Inform the cell
  code <- newRef . rtBox $ RtInformCell (rtVar 0) (rtInt 5)
  rtEval @IO @IORef code (Vec.fromList [x]) >>= readRef >>= \case
    RtCell cellRef _ _ -> do
      readRef cellRef >>= \case
        RtWhnf (RtConF 2 topResult) -> do
          readRef topResult >>= \case
            RtWhnf (RtPrimF y) -> y @?= RtInt 5
            RtWhnf _ -> error "Some other whnf"
            RtCell _ _ _ -> error "Some cell"
            RtThunk _ _ -> error "Some thunk"
          -- _ -> error "Expecting Whnf"
          pure ()
        _ -> error "Expecting Top"
  -- Inform it again, should cause a conflict
  code' <- newRef . rtBox $ RtInformCell (rtVar 0) (rtInt 5)
  try (rtEval @IO @IORef code' (Vec.fromList [x])) >>= \case
    Left (SomeException _) -> pure ()
    Right _ -> error "Repeated inform should have generated a conflict!"

testInformFillsValue = testCase "inform fills value" $ do
  -- Setup the cell
  x <- newRef . rtBox $ rtCell identityLat
  -- Ensure that the created cell is empty
  rtEval @IO @IORef x mempty >>= readRef >>= \case
    RtCell cellRef _ _ -> do
      readRef cellRef >>= \case
        RtWhnf (RtConF 0 _) -> pure ()
        _ -> error "Expecting Empty"
  -- Inform the cell
  code <- newRef . rtBox $ RtInformCell (rtVar 0) (rtInt 5)
  _ <- rtEval @IO @IORef code (Vec.fromList [x])
  -- Ensure that the cell is now filled
  readRef x >>= \case
    RtCell cellRef _ _ -> do
      normalize cellRef >>= \case
        RtCon 2 _ -> pure ()
        _ -> error $ "Expecting Top"

testAddingCellDep =
  testCase "can add propagator as dependent of cell" $ do
    -- Setup the cell
    let xCell = rtCell identityLat
    let yCell = rtCell identityLat
    x <- newEmptyCell xCell
    y <- newEmptyCell yCell
    -- Inform the cell
    code <- newRef . rtBox . RtAddCellDep (rtVar 0) $ rtProp (rtVar 1) [rtVar 0] idProp
    void $ rtEval @IO @IORef code (Vec.fromList [x, y])

testTriggerProp = testCase "can add propagator as dependent of cell" $ do
  -- Setup the cell
  let xCell = rtCell identityLat
  let yCell = rtCell identityLat
  x <- newEmptyCell xCell
  y <- newEmptyCell yCell
  -- Inform the cell
  code1 <- newRef . rtBox . RtAddCellDep (rtVar 0) $ rtProp (rtVar 1) [rtVar 0] idProp
  void $ rtEval @IO @IORef code1 (Vec.fromList [x, y])
  code2 <- newRef . rtBox $ RtInformCell (rtVar 0) (rtInt 5)
  void $ rtEval @IO @IORef code2 (Vec.fromList [x])
  -- Ensure y was filled from propagation
  readRef y >>= \case
    RtCell cellRef _ _ -> do
      readRef cellRef >>= \case
        RtWhnf (RtConF 2 _) -> pure ()
        _ -> error "Cell y was not filled!"
    _ -> error "Expecting Cell"

{-
testLexer :: IO ()
testLexer = do
  let fileName = "examples/Example1.jy"
  txt <- Text.readFile fileName
  case lexer fileName txt of
    Right tokens -> putStrLn $ concat $ displayToken <$> tokens
    Left e -> putStrLn $ show e
-}
