{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Exception
import           Data.Functor.Foldable
import qualified Data.Map.Strict               as Map
import           Polysemy
import           Polysemy.Error
import           Test.Tasty
import           Test.Tasty.Hspec

import           Core
import           Expr
import           Typed
import           Core.TypeCheck


main :: IO ()
main = defaultMain =<< tests


tests :: IO TestTree
tests = testGroup "Tests" <$> sequence [unitTests]

unitTests :: IO TestTree
unitTests = testGroup "Unit Tests" <$> sequence [typeCheckingTests]

typeCheckingTests :: IO TestTree
typeCheckingTests = testSpec "Type Checking" $ do
  describe "check" $ do

    let shouldAccept = either (const False) (const True)
    let shouldReject = either (const True) (const False)
    let runCheck     = run . runError @SomeException

    it "Accepts Free Variables in Context" $ do
      let tbl              = Map.fromList [("x", mkCon ce "Thing")]
      let (e :: Fix CoreE) = mkFree ce "x"
      let (t :: Fix CoreE) = (mkCon ce "Thing")
      shouldAccept . runCheck $ check runNoLogging tbl e t

    it "Rejects Free Variables not in Context" $ do
      let tbl              = mempty
      let (e :: Fix CoreE) = mkFree ce "x"
      let (t :: Fix CoreE) = (mkCon ce "Thing")
      shouldReject . runCheck $ check runNoLogging tbl e t

    it "Accepts Lambdas" $ do
      let tbl              = mempty
      let (e :: Fix CoreE) = mkLam ce () (mkVar ce 0)
      let (t :: Fix CoreE) = mkArrow ce
                                     (Inline R1, Inline L)
                                     (mkCon ce "Thing")
                                     (mkCon ce "Thing")
      shouldAccept . runCheck $ check runNoLogging tbl e t

    it "Accepts Valid Application" $ do
      let tbl = Map.fromList [("x", mkCon ce "Thing")]
      let (e :: Fix CoreE) =
            mkApp ce (mkLam ce () (mkVar ce 0)) (mkFree ce "x")
      let (t :: Fix CoreE) = mkCon ce "Thing"
      shouldAccept . runCheck $ check runNoLogging tbl e t

    it "Rejects Invalid Application" $ do
      let tbl = Map.fromList [("x", mkCon ce "Foo")]
      let (e :: Fix CoreE) =
            mkApp ce (mkLam ce () (mkVar ce 0)) (mkFree ce "x")
      let (t :: Fix CoreE) = mkCon ce "Thing"
      shouldReject . runCheck $ check runNoLogging tbl e t
