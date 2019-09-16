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

module Core.TypeCheck.Solve where

import           Control.Monad
import           Prelude                 hiding ( log )
import           Lens.Micro.Platform
import           Data.Bifunctor
import           Control.Monad.Catch.Pure
import           Control.Monad.State.Strict
import           Data.Functor.Foldable
import           Numeric.Natural

import           Constraint
import           Core
import           Display
import           Subst
import           Expr
import           Env
import           Typed
import           Summable
import           Core.TypeCheck.Check
import           Core.TypeCheck.Unify
import           Core.TypeCheck.Constrain

solveConstraints
  :: (Logging m, MonadState ConstraintST m, MonadThrow m, NameGen m) => m ()
solveConstraints = do
  st <- get
  let w = st ^? ctx . constraints . _head
  case w of
    Just w' -> do
      modify $ ctx . constraints %~ tail
      case w' of
        Flat (EqC x y) -> do
          applyUnify [(x, y)]
          solveConstraints
        _ -> undefined
    Nothing -> log "All constraints solved!"
 where

  applyUnify
    :: (Logging m, MonadState ConstraintST m, MonadThrow m, NameGen m)
    => [(Fix CheckE, Fix CheckE)]
    -> m ()
  applyUnify []                      = pure ()
  applyUnify ((Fix x, Fix y) : rest) = case (x, y) of
    -- Valid Cases
    (Here a, Here b) -> runUnify a b rest
    (There (Here a), There (Here b)) -> runUnify a b rest
    (There (There (Here a)), Here b) -> runUnify a b rest
    (There (There (Here a)), There (There (Here b))) -> runUnify a b rest
    (There (There (Here a)), There (Here b)) -> runUnify a b rest
    (Here a, There (There (Here b))) -> runUnify b a rest
    (There (Here a), There (There (Here b))) -> runUnify b a rest

    -- Invalid Cases
    (Here _, There (Here _)) -> error "Can't Unify an expr with a type term"
    (There (Here _), Here _) -> error "Can't Unify a type with an expr term"
    _ -> error "Invalid Unify"

  runUnify
    :: ( Display (f String)
       , Display (g String)
       , g :<: '[Expr Check, Typed Check, Checked]
       , f :<: '[Expr Check, Typed Check, Checked]
       , Logging m
       , MonadState ConstraintST m
       , MonadThrow m
       , Unify f g
       , NameGen m
       )
    => f (Fix CheckE)
    -> g (Fix CheckE)
    -> [(Fix CheckE, Fix CheckE)]
    -> m ()
  runUnify a b rest = do
    log $ "Unifying: " <> displayF a <> " and " <> displayF b
    unify a b >>= \case
      Left (MkFill f) -> do
        let b' = Fix . inj $ b
        log $ "Filling hole: " <> f <> " with " <> cata display b'
        -- Apply the substitution to all constraints
        modify $ ctx . constraints %~ (fmap . fmap) (subst b' (MkFill f))

        st <- get
        log $ "Constraints After Fill: "
        mapM_ log $ displayF <$> (st ^. ctx . constraints)

        -- Apply the subst to rest of unification nodes and continue solving
        let sub   = subst b' (MkFill f)
        let rest' = (fmap sub <$> rest)

        log $ "Sub terms to unify:"
        mapM_ (log . show) $ bimap (cata display) (cata display) <$> rest'

        applyUnify rest'

      Right more -> do
        applyUnify =<< mapM reduceSubTerm more
        applyUnify rest

  reduceSubTerm
    :: (Logging m, NameGen m)
    => SubTerm (Fix CheckE)
    -> m (Fix CheckE, Fix CheckE)
  reduceSubTerm (SubTerm SubEq  x y) = pure (x, y)
  reduceSubTerm (SubTerm SubArr x y) = do
    dep <- hole <$> newName
    log
      $  "Applying hole "
      <> cata display dep
      <> " to expression "
      <> cata display y
    let y' = subst dep (0 :: Natural) y
    pure (x, y')

