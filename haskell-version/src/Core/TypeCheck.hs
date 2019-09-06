{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
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
import           Data.Proxy
import           Numeric.Natural

import           Constraint
import           Core
import           Subst
import           Env
import           Expr
import           Typed
import           Summable

data Checked a = Hole String deriving (Functor)

data TypeCheckError

type CheckE = Summed '[ Expr Check, Typed Check, Checked]

data Check

instance Expression Check where
  type LamOpts Check = LamOpts Core

instance TypedExpression Check where
  type RigName Check = RigName Core
  type PolName Check = RigName Core
  type ArrowOpts Check = ( Either (Abst Rig) (Fix CheckE)
                         , Either (Abst Pol) (Fix CheckE)
                         , Either (Abst Pol) (Fix CheckE)
                         )


newtype Rigs = Rigs (Map String Rig)

instance Semigroup Rigs where
 (Rigs x) <> (Rigs y) = Rigs $ Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (const (<>))) x y

instance Monoid Rigs where
  mempty = Rigs mempty

data Ctx = Ctx ([W (Fix CheckE)]) Rigs

instance Semigroup Ctx where
  (Ctx a b) <> (Ctx c d) = Ctx (a <> c) (b <> d)

instance Monoid Ctx where
  mempty = Ctx mempty mempty

hole :: String -> Fix CheckE
hole = Fix . inj . Hole

genConstraints :: (NameGen m) => Fix CoreE -> m (Fix CheckE, Ctx)
genConstraints = cata go
 where
  go (Here layer) =
    let (TypeBuilder _ _ arrow _ _) = typeBuilder (Proxy @Check)
    in  case layer of

          (App x y) -> do
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
          (Lam _ body) -> error "genConstraints lam not implemented"
          (Val _     ) -> error "genConstraints val not implemented"

  go (There (Here layer)) =
    let _ = undefined
    in  case layer of
          (RArr _ _  ) -> error "genConstraints rig not implemented"
          (PArr _ _  ) -> error "genConstraints pol not implemented"
          (TArr _ _ _) -> error "genConstraints arr not implemented"
          (TCon _    ) -> error "genConstraints con not implemented"
          (Type _    ) -> error "genConstraints type not implemented"

  go _ = undefined

solveConstraints :: MonadThrow m => [W (Fix CheckE)] -> m ()
solveConstraints = undefined

check :: MonadThrow m => Fix CoreE -> m (Cofree CoreE Ctx)
check = undefined
