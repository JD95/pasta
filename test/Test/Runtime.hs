{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Test.Runtime where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logic
import Data.IORef
import Data.List.NonEmpty
import Debug.Trace
import Runtime
import Runtime.Logic
import Runtime.Prop
import Runtime.Ref
import Runtime.Term
import Runtime.Types
import System.Environment
import System.Mem.StableName
import Test.Tasty
import Test.Tasty.HUnit

tests = testGroup "runtime" [unification]

unification =
  testGroup
    "unification"
    [ testCase "inform fills cell" $
        observeT $ do
          x <- cell @IORef @StableName Nothing
          inform x $ Just 1
          readCell x >>= liftIO . (@?= Just 1),
      testCase "ints unify" $
        observeT $ do
          x <- cell @IORef @StableName (Just 1)
          y <- cell @IORef @StableName Nothing
          unify x y
          readCell x >>= liftIO . (@?= Just 1),
      testCase "mismatch fails" $ do
        x <- cell @IORef @StableName (Just 1)
        y <- cell @IORef @StableName (Just 2)
        expectException $ unify x y,
      testCase "prop works" $
        observeT $ do
          x <- cell @IORef @StableName Nothing
          z <- cell @IORef @StableName (Nothing, Nothing)
          prop [Watched z] x $ fst <$> readCell z
          inform z (Just 1, Nothing :: Maybe Int)
          readCell x >>= liftIO . (@?= Just 1),
      testCase "products propagate to children" $
        observeT $ do
          x <- cell @IORef @StableName Nothing
          y <- cell @IORef @StableName Nothing
          z <- cell @IORef @StableName (Nothing, Nothing)

          prop [Watched z] x $ do
            fst <$> readCell z

          readRef (triggerWatchers z) >>= liftIO . ((@?= 1) . Prelude.length)

          prop [Watched z] y $ do
            snd <$> readCell z

          readRef (triggerWatchers z) >>= liftIO . ((@?= 2) . Prelude.length)

          inform z (Just 1, Nothing)

          readRef (triggerWatchers z) >>= liftIO . ((@?= 2) . Prelude.length)

          inform z (Nothing, Just 2)

          readCell y >>= liftIO . (@?= Just 2)
          readCell x >>= liftIO . (@?= Just 1)
    ]

expectException action = do
  try action >>= \case
    Right _ -> assertFailure "Was expecting exception!"
    Left (SomeException _) -> pure ()
