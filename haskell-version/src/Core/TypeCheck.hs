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
import           Control.Monad.Catch.Pure
import           Control.Monad.State.Strict
import           Data.Functor.Foldable

import           Constraint
import           Core
import           Expr
import           Env
import           Typed
import           Core.TypeCheck.Constrain
import           Core.TypeCheck.Solve

newtype SolveM a
  = SolveM
  { runSolve :: StateT Ctx (CatchT IO) a
  } deriving (Functor, Applicative, Monad, MonadState Ctx, MonadThrow, MonadIO)

instance Logging SolveM where
  log = liftIO . putStrLn

check :: Fix CoreE -> Fix CoreE -> IO (Either SomeException ())
check e goal = do
  let (ty, st) = runState (genConstraints e) initConstraintST
  let cs       = (st ^. ctx) & constraints %~ (:) (ty ~: toCheck goal)
  runCatchT $ evalStateT (runSolve solveConstraints) cs

testCheck :: IO ()
testCheck = do
  let (e :: Fix CoreE) = mkLam ce () (mkLam ce () (mkVar ce 1))
  let (t :: Fix CoreE) =
        mkArrow ce (Inline R0, Inline L) (mkCon ce "Thing") $
          mkArrow ce (Inline R0, Inline L) (mkCon ce "Foo") (mkCon ce "Thing")
  print =<< check e t
