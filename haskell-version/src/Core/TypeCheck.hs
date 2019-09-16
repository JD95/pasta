{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.TypeCheck where

import           Lens.Micro.Platform
import           Data.Functor.Foldable   hiding ( embed )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Numeric.Natural
import           Polysemy
import           Polysemy.State
import           Polysemy.Error

import           Constraint
import           Core
import           Display
import           Expr
import           Env
import           Subst
import           Typed
import           Core.TypeCheck.Check
import           Core.TypeCheck.Constrain
import           Core.TypeCheck.Solve
import           Core.TypeCheck.Check
import           Core.TypeCheck.Unify

runConstraintGenAsST
  :: (Member (State ConstraintST) r) => Sem (ConstraintGen ': r) a -> Sem r a
runConstraintGenAsST = interpretH $ \case
  Require w -> do
    modify $ \st -> st & ctx . constraints %~ ((:) w)
    pureT ()
  Usage s r -> do
    modify $ \st ->
      let f Nothing   = Just r
          f (Just r') = Just (r <> r')
      in  st & ctx . rigs %~ (Rigs . Map.alter f s . unRigs)
    pureT ()
  WithBinding x action -> do
    action' <- runT action

    let runIt = raise . runConstraintGenAsST
    stOld <- get
    modify $ ctx . bindings %~ (:) x
    result <- runIt action'
    modify $ ctx . bindings .~ (stOld ^. ctx . bindings)
    pure result

  LookupBinding n -> do
    st <- get
    pureT $ st ^? ctx . bindings . ix (fromIntegral n)

runLoggingIO :: (Member (Embed IO) r) => Sem (Logging ': r) a -> Sem r a
runLoggingIO = interpret $ \case
  Log s -> embed $ putStrLn s

check
  :: Map String (Fix CoreE)
  -> Fix CoreE
  -> Fix CoreE
  -> IO (Either UnifyException ())
check tbl e goal = do
  let (st, ty) =
        run
          . runState initConstraintST
          . runNameGenAsState
          . runConstraintGenAsST
          $ genConstraints tbl e
  let cs = st & ctx . constraints %~ (:) (ty ~: toCheck goal)
  runM
    . runError
    . runLoggingIO
    . evalState cs
    . runNameGenAsState
    $ solveConstraints
  -- runCatchT $ evalStateT (runSolve solveConstraints) cs

testCheck :: IO ()
testCheck = do
  let tbl              = Map.fromList $ [("x", mkCon ce "Foo")]
  let (e :: Fix CoreE) = mkApp ce (mkLam ce () (mkVar ce 0)) (mkFree ce "x")
  let (t :: Fix CoreE) = mkCon ce "Thing"
  print =<< check tbl e t
foo = cata display $ subst
  (hole "a")
  (0 :: Natural)
  (mkArrow cke (Left $ Inline R0, Left $ Inline L) (mkVar cke 0) (mkVar cke 1))
