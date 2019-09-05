{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.TypeCheck where

import           Control.Comonad.Cofree
import           Control.Monad.Catch.Pure
import           Control.Monad.Reader
import           Data.Functor.Const
import           Data.Functor.Foldable
import qualified Data.Map.Merge.Strict         as Map
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Void
import           Numeric.Natural

import           Constraint
import           Core
import           Core.Subst
import           Env
import           Expr
import           Typed

data Checked (f :: * -> *) a = Hole String

data TypeCheckError

newtype CheckCore a = MkCheckCore { unCheckCore :: Expr Check (Typed Check (Checked (Const Void))) a }

data Check

instance Expression Check where
  type LamOpts Check = LamOpts Core
  type ExprExt Check f = Typed Check (TypedExt Check f)

instance TypedExpression Check where
  type RigName Check = RigName Core
  type PolName Check = RigName Core
  type ArrowOpts Check = ( Either (Abst Rig) (Fix CheckCore)
                         , Either (Abst Pol) (Fix CheckCore)
                         , Either (Abst Pol) (Fix CheckCore)
                         )
  type TypedExt Check f = Checked (Const Void)

instance ExprConst Check CheckCore where
  injExpr = MkCheckCore

instance TypedConst Check CheckCore where
  injTyped = MkCheckCore . Expr

newtype Rigs = Rigs (Map String Rig)

instance Semigroup Rigs where
 (Rigs x) <> (Rigs y) = Rigs $ Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (const (<>))) x y

instance Monoid Rigs where
  mempty = Rigs mempty

data Ctx = Ctx ([W (Fix CheckCore)]) Rigs

instance Semigroup Ctx where
  (Ctx a b) <> (Ctx c d) = Ctx (a <> c) (b <> d)

instance Monoid Ctx where
  mempty = Ctx mempty mempty

hole :: String -> Fix CheckCore
hole = Fix . injTyped . Typed . Hole

genConstraints :: (NameGen m) => Fix CoreE -> m (Fix CheckCore, Ctx)
genConstraints = cata (goExpr . unCoreE)
 where
  goExpr (App x y) = do
    (xTy, xCtx) <- x
    (yTy, yCtx) <- y
    inTy        <- hole <$> newName
    outTy       <- hole <$> newName
    -- I need some way to generate constraints for the
    -- arrow opts. In other words, how to constrain rig
    -- and pol values?
    --
    -- Rig values need usage counts
    -- Pol values need inital laziness or strictness info from functions
    funRig      <- hole <$> newName
    funInPol    <- hole <$> newName
    funOutPol   <- hole <$> newName
    let opts = (Right funRig, Right funInPol, Right funOutPol)
    let new =
          [ yTy ~: inTy
          , xTy ~: arrow opts inTy outTy
          , isRig funRig
          , isPol funInPol
          , isPol funOutPol
          ]
    pure $ (outTy, xCtx <> yCtx <> Ctx new mempty)
  goExpr (Lam _ body) = error "genConstraints lam not implemented"
  goExpr (Val  _    ) = error "genConstraints val not implemented"
  goExpr (Expr x    ) = goType x

  goType (RArr _ _  ) = error "genConstraints rig not implemented"
  goType (PArr _ _  ) = error "genConstraints pol not implemented"
  goType (TArr _ _ _) = error "genConstraints arr not implemented"
  goType (TCon  _   ) = error "genConstraints con not implemented"
  goType (Type  _   ) = error "genConstraints type not implemented"
  goType (Typed _   ) = undefined

solveConstraints :: MonadThrow m => [W (Fix CheckCore)] -> m ()
solveConstraints = undefined

check :: MonadThrow m => Fix CoreE -> m (Cofree CoreE Ctx)
check = undefined
