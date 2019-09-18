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

import           Control.Monad
import           Lens.Micro.Platform
import           Data.Functor.Foldable   hiding ( embed )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Numeric.Natural
import           Control.Exception              ( SomeException(..) )
import           Polysemy
import           Polysemy.State
import           Polysemy.Error

import           Constraint
import           Core
import           Expr
import           Env
import           Typed
import           Core.TypeCheck.Check
import           Core.TypeCheck.Constrain
import           Core.TypeCheck.Solve
import           Core.TypeCheck.SubstTable
import           Core.TypeCheck.Unify

initNames :: Names
initNames = go
  (zipWith (\l n -> l : show @Natural n)
           (cycle ['a' .. 'z'])
           (join $ replicate 26 <$> [1 ..])
  )
 where
  go (x : xs) = Next x (go xs)
  go []       = undefined

runNameGenAsState :: Sem (NameGen ': r) a -> Sem (State Names ': r) a
runNameGenAsState = reinterpret $ \case
  NewName -> do
    st <- get
    let (next, rest) = popName st
    put rest
    pure next

runConstraintGenAsST
  :: (Member (State Ctx) r) => Sem (ConstraintGen ': r) a -> Sem r a
runConstraintGenAsST = interpretH $ \case
  Require w -> do
    modify $ \st -> st & constraints %~ ((:) w)
    pureT ()
  Usage s r -> do
    modify $ \st ->
      let f Nothing   = Just r
          f (Just r') = Just (r <> r')
      in  st & rigs %~ (Rigs . Map.alter f s . unRigs)
    pureT ()
  WithBinding x action -> do
    action' <- runT action

    let runIt = raise . runConstraintGenAsST
    stOld <- get
    modify $ bindings %~ (:) x
    result <- runIt action'
    modify $ bindings .~ (stOld ^. bindings)
    pure result

  LookupBinding n -> do
    st <- get
    pureT $ st ^? bindings . ix (fromIntegral n)

runNoLogging :: Sem (Logging ': r) a -> Sem r a
runNoLogging = interpret $ \case
  Log _ -> pure ()

runLoggingIO :: (Member (Embed IO) r) => Sem (Logging ': r) a -> Sem r a
runLoggingIO = interpret $ \case
  Log s -> embed $ putStrLn s

check
  :: (Members '[Error SomeException] r)
  => (forall a . Sem (Logging ': r) a -> Sem r a)
  -> Map String (Fix CoreE)
  -> Fix CoreE
  -> Fix CoreE
  -> Sem r ()
check logInterp tbl e goal = do
  (ns, (ctx, ty)) <-
    runState @Names initNames
    . runNameGenAsState
    . runState @Ctx mempty
    . mapError @ConstrainError SomeException
    . runConstraintGenAsST
    $ genConstraints tbl e
  let cs = ctx & constraints %~ (:) (ty ~: toCheck goal)
  logInterp
    . mapError @UnifyException SomeException
    . evalState (MkSubstTable @Fill @(Fix CheckE) mempty)
    . evalState cs
    . evalState @Names ns
    . runNameGenAsState
    $ solveConstraints

testCheck :: IO ()
testCheck = do
  let tbl              = Map.fromList $ [("x", mkCon ce "Thing")]
  let (e :: Fix CoreE) = mkApp ce (mkLam ce () (mkVar ce 0)) (mkFree ce "x")
  let (t :: Fix CoreE) = mkCon ce "Thing"
  print =<< (runM . runError @SomeException $ (check runLoggingIO tbl e t))
