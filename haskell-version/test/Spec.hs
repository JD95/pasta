{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Functor.Foldable
import qualified Data.Map.Strict as Map
import           Polysemy
import           Test.Tasty
import           Test.Tasty.Hspec

import           Core
import           Expr
import           Typed
import           Core.TypeCheck


main :: IO ()
main = defaultMain =<< tests


tests :: IO TestTree
tests = testGroup "Tests" . pure <$> unitTests

unitTests :: IO TestTree
unitTests = testSpec "Unit Tests" $ do
  describe "Type Checking" $ do
      it "Accepts Valid Application" $ do
        let tbl              = Map.fromList $ [("x", mkCon ce "Thing")]
        let (e :: Fix CoreE) = mkApp ce (mkLam ce () (mkVar ce 0)) (mkFree ce "x")
        let (t :: Fix CoreE) = mkCon ce "Thing"
        either (const False) (const True) $ run (check runNoLogging tbl e t)

      it "Rejects Invalid Application" $ do
        let tbl              = Map.fromList $ [("x", mkCon ce "Foo")]
        let (e :: Fix CoreE) = mkApp ce (mkLam ce () (mkVar ce 0)) (mkFree ce "x")
        let (t :: Fix CoreE) = mkCon ce "Thing"
        either (const True) (const False) $ run (check runNoLogging tbl e t) 
