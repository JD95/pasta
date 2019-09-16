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
import           Data.Functor.Foldable
import           Numeric.Natural
import           Polysemy
import           Polysemy.Reader
import           Polysemy.Error
import           Polysemy.State
import           Control.Exception              ( SomeException )

import           Constraint
import           Core
import           Core.TypeCheck.Check
import           Core.TypeCheck.Constrain
import           Core.TypeCheck.SubstTable
import           Core.TypeCheck.Unify
import           Display
import           Env
import           Expr
import           Subst
import           Summable
import           Typed

type SubstTbl = SubstTable Fill (Fix CheckE)

solveConstraints
  :: ( Members
         '[State SubstTbl, Logging, State ConstraintST, Error UnifyException, NameGen]
         r
     )
  => Sem r ()
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
        _ -> solveConstraints
    Nothing -> log "All constraints solved!"

rewriteHole :: (Member (State SubstTbl) r) => Fix CheckE -> Sem r (Fix CheckE)
rewriteHole (Fix (There (There (Here (Hole s))))) = do
  lookupSubst (MkFill s) <$> get >>= \case
    Just e  -> pure e
    Nothing -> pure $ hole s
rewriteHole other = pure other

applyUnify
  :: ( Members
         '[Logging, State SubstTbl, State ConstraintST, Error UnifyException, NameGen]
         r
     )
  => [(Fix CheckE, Fix CheckE)]
  -> Sem r ()
applyUnify []                = pure ()
applyUnify ((x', y') : rest) = do
  Fix x <- rewriteHole x'
  Fix y <- rewriteHole y'

  case (x, y) of
    -- Valid Cases
    (Here a, Here b) -> runUnify a b rest
    (There (Here a), There (Here b)) -> runUnify a b rest

    (There (There (Here a)), Here b) -> runUnify a b rest
    (There (There (Here a)), There (Here b)) -> runUnify a b rest
    (Here a, There (There (Here b))) -> runUnify b a rest
    (There (Here a), There (There (Here b))) -> runUnify b a rest
    (There (There (Here a)), There (There (Here b))) -> runUnify a b rest

    -- Invalid Cases
    (Here _, There (Here _)) -> error "Can't Unify an expr with a type term"
    (There (Here _), Here _) -> error "Can't Unify a type with an expr term"
    _ -> error "Invalid Unify"

runUnify
  :: ( Display (f String)
     , Display (g String)
     , g :<: '[Expr Check, Typed Check, Checked]
     , f :<: '[Expr Check, Typed Check, Checked]
     , Members
         '[Logging, State SubstTbl, State ConstraintST, Error UnifyException, NameGen]
         r
     , Unify f g
     )
  => f (Fix CheckE)
  -> g (Fix CheckE)
  -> [(Fix CheckE, Fix CheckE)]
  -> Sem r ()
runUnify a b rest = do
  log $ "Unifying: " <> displayF a <> " and " <> displayF b
  unify a b >>= \case
    Left (MkFill f) -> do
      let b' = Fix . inj $ b
      log $ "Filling hole: " <> f <> " with " <> cata display b'
      modify $ rewrite (MkFill f) b'

      -- Apply the subst to rest of unification nodes and continue solving
      log $ "Sub terms to unify:"
      mapM_ (log . show) $ bimap (cata display) (cata display) <$> rest

      applyUnify rest

    Right more -> do
      applyUnify =<< mapM reduceSubTerm more
      applyUnify rest

reduceSubTerm
  :: (Members '[Logging, NameGen] r)
  => SubTerm (Fix CheckE)
  -> Sem r (Fix CheckE, Fix CheckE)
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

