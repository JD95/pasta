{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Exception
import qualified Data.Map.Strict               as Map
import           Data.Functor.Foldable
import           Polysemy
import           Polysemy.State
import           Polysemy.Error
import           Test.Tasty
import           Test.Tasty.Hspec

import           Core
import           Expr
import           Typed
import           Core.TypeCheck
import           Core.TypeCheck.Check
import           Core.TypeCheck.Unify
import           Core.TypeCheck.Constrain

import           Stats

shouldAccept = either (const False) (const True)
shouldReject = either (const True) (const False)
shouldProduce x = either (const False) (== x)

main :: IO ()
main =
  defaultMainWithIngredients (consoleStatsReporter : defaultIngredients)
    =<< tests

tests :: IO TestTree
tests = testGroup "Tests" <$> sequence [unitTests]

unitTests :: IO TestTree
unitTests =
  testGroup "Unit Tests" <$> sequence [typeCheckingTests, unificationTests]

unificationTests :: IO TestTree
unificationTests = testSpec "Unification" . parallel $ do
  describe "unification" $ do
    let runUnify x y =
          fmap snd
            . run
            . runError @SomeException
            . mapError @UnifyException SomeException
            . runState @Names initNames
            . runNameGenAsState
            $ unify x y

    describe "ListH" $ do

      it "equal ListH values should unify" $ do
        let (x :: Checked (Fix CheckE)) =
              ListH $ Map.fromList [(0, mkCon cke "Foo"), (1, mkCon cke "Bar")]
        shouldAccept $ runUnify x x

      it "empty ListH values should unify" $ do
        let (x :: Checked (Fix CheckE)) = ListH $ Map.fromList []
        shouldAccept $ runUnify x x

      it "non-equal ListH values should not unify" $ do
        let t1 = mkCon cke "Foo"
        let t2 = mkCon cke "Boo"
        let (x :: Checked (Fix CheckE)) =
              ListH $ Map.fromList [(0, t1), (1, t1)]
        let (y :: Checked (Fix CheckE)) = ListH $ Map.fromList [(1, t2)]
        let expected                    = Right [SubTerm SubEq t1 t2]
        shouldProduce expected $ runUnify x y

typeCheckingTests :: IO TestTree
typeCheckingTests = testSpec "Type Checking" . parallel $ do
  describe "check" $ do

    let runCheck = run . runError @SomeException

    describe "free variables" $ do

      it "accept if in context" $ do
        let tbl = Map.fromList [("x", mkCon ce "Thing")]
        let e   = mkFree ce "x"
        let t   = mkCon ce "Thing"
        shouldAccept . runCheck $ check runNoLogging tbl e t

      it "reject if not in context" $ do
        let tbl = mempty
        let e   = mkFree ce "x"
        let t   = mkCon ce "Thing"
        shouldReject . runCheck $ check runNoLogging tbl e t

    describe "lambdas" $ do

      it "accept valid lambda" $ do
        let tbl = mempty
        let e   = mkLam ce () (mkVar ce 0)
        let
          t = mkArrow ce
                      (Inline R1, Inline L)
                      (mkCon ce "Thing")
                      (mkCon ce "Thing")
        shouldAccept . runCheck $ check runNoLogging tbl e t

      it "reject inconsistent input use" $ do
        let tbl = Map.fromList
              [ ( "f"
                , mkArrow'
                  ce
                  [ ((Inline RU, Inline S), mkCon ce "Thing")
                  , ((Inline RU, Inline S), mkCon ce "Foo")
                  ]
                  (mkCon ce "Thing")
                )
              ]
        let e =
              mkLam ce () $ mkApp' ce (mkFree ce "f") [mkVar ce 0, mkVar ce 0]
        let
          t = mkArrow ce
                      (Inline RU, Inline S)
                      (mkCon ce "Thing")
                      (mkCon ce "Thing")
        shouldReject . runCheck $ check runNoLogging tbl e t

    describe "application" $ do

      it "accept matching input" $ do
        let tbl = Map.fromList [("x", mkCon ce "Thing")]
        let e = mkApp ce (mkLam ce () (mkVar ce 0)) (mkFree ce "x")
        let t   = mkCon ce "Thing"
        shouldAccept . runCheck $ check runNoLogging tbl e t

      it "reject mismatched input" $ do
        let tbl = Map.fromList [("x", mkCon ce "Foo")]
        let e = mkApp ce (mkLam ce () (mkVar ce 0)) (mkFree ce "x")
        let t   = mkCon ce "Thing"
        shouldReject . runCheck $ check runNoLogging tbl e t

    describe "products" $ do

      it "accepts projections" $ do
        let tbl =
              Map.fromList [("x", mkProdTy ce [mkCon ce "Foo", mkCon ce "Bar"])]
        let e = mkApp ce (mkProj ce (Index 0)) (mkFree ce "x")
        let t = mkCon ce "Foo"
        shouldAccept . runCheck $ check runNoLogging tbl e t


    describe "co-products" $ do

      it "accepts injection" $ do
        let tbl = Map.fromList [("x", mkCon ce "Foo")]
        let e   = mkInj ce (Index 0) (mkFree ce "x")
        let t   = mkCoProd ce [mkCon ce "Foo", mkCon ce "Bar"]
        shouldAccept . runCheck $ check runNoLogging tbl e t
