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

import           Data.Functor.Identity
import           Control.Monad.Catch.Pure
import           Control.Monad.State.Strict
import           Data.Functor.Foldable

import           Constraint
import           Core
import           Expr
import           Typed
import           Core.TypeCheck.Constrain
import           Core.TypeCheck.Solve

check :: Fix CoreE -> Fix CoreE -> Either SomeException ()
check e goal =
  let (ty, (Ctx ws rs, _)) =
        runState (genConstraints e) (Ctx [] mempty, initNames)
  in  runIdentity $ runCatchT $ evalStateT solveConstraints
                                           (Ctx (ty ~: toCheck goal : ws) rs)

testCheck = do
  let (e :: Fix CoreE) = mkVar ce 0
  let (t :: Fix CoreE) = mkCon ce "Thing"
  print $ check e t
