{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.TypeCheck.Constrain where

import           Control.Exception              ( Exception(..) )
import           Lens.Micro.Platform
import           Data.Functor.Foldable
import qualified Data.Map.Merge.Strict         as Map
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Numeric.Natural
import           Polysemy
import           Polysemy.Error

import           Constraint
import           Core
import           Env
import           Expr
import           Typed
import           Summable
import           Core.TypeCheck.Check

newtype Rigs = Rigs { unRigs :: Map String Rig }

instance Semigroup Rigs where
 (Rigs x) <> (Rigs y) = Rigs $
   Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched (const (<>))) x y

instance Monoid Rigs where
  mempty = Rigs mempty

data Ctx
  = Ctx
  { _constraints :: ([W (Fix CheckE)])
  , _rigs :: Rigs
  , _bindings :: [Fix CheckE]
  }

makeLenses ''Ctx

instance Semigroup Ctx where
  (Ctx a b e) <> (Ctx c d f) = Ctx (a <> c) (b <> d) (e <> f)

instance Monoid Ctx where
  mempty = Ctx mempty mempty mempty

data Names = Next String Names

popName :: Names -> (String, Names)
popName (Next n ns) = (n, ns)

data ConstraintGen m a where
  Require :: W (Fix CheckE) -> ConstraintGen m ()
  Usage :: String -> Rig -> ConstraintGen m ()
  WithBinding :: Fix CheckE -> m a -> ConstraintGen m a
  LookupBinding :: Natural -> ConstraintGen m (Maybe (Fix CheckE))

makeSem ''ConstraintGen

data ConstrainError
  = UndefinedSymbol String
    deriving (Eq, Show)

instance Exception ConstrainError

genConstraints
  :: (Members '[ConstraintGen, NameGen, Error ConstrainError] r)
  => (Map String (Fix CoreE))
  -> Fix CoreE
  -> Sem r (Fix CheckE)
genConstraints tbl = cata go
 where
  go (Here layer) = case layer of

    (Val (Bound i)) -> do
      lookupBinding i >>= \case
        Just term -> pure term
        Nothing   -> error "Binding has no binder!"
    (Val (Free name)) -> case Map.lookup name tbl of
      Just result -> pure (toCheck result)
      Nothing     -> throw $ UndefinedSymbol name
    (Val (Inline x)) -> x

    (Case _ _) -> error "Constraining case expressions not implemented"
    (Inj (Index i) val) -> do
      ty <- val
      pure $ listH (Map.singleton i ty)
    (Inj _ _       ) -> error "Constraining injections not implemented"
    (Proj (Index i)) -> do
      ty     <- hole <$> newName
      funRig <- hole <$> newName
      funPol <- hole <$> newName
      let opts = (Right funRig, Right funPol)
      pure $ mkArrow cke opts (listH (Map.singleton i ty)) ty
    (Proj _    ) -> error "Constraining projections not implemented"
    (List _ _  ) -> error "Constraining lists not implemented"
    (Record _  ) -> error "Constraining records not implemented"

    (Lam _ body) -> do
      inTy   <- hole <$> newName
      funRig <- hole <$> newName
      funPol <- hole <$> newName
      outTy  <- withBinding inTy body
      let opts = (Right funRig, Right funPol)
      pure $ mkArrow cke opts inTy outTy

    (App x y) -> do
      funTy  <- x
      inTy   <- y
      funRig <- hole <$> newName
      funPol <- hole <$> newName
      outTy  <- hole <$> newName
      let opts = (Right funRig, Right funPol)
      require $ funTy ~: mkArrow cke opts inTy outTy
      -- require $ isRig funRig
      -- require $ isPol funPol
      pure outTy

  go (There (Here layer)) = case layer of
    (RArr _ _   ) -> error "genConstraints rig not implemented"
    (PArr _ _   ) -> error "genConstraints pol not implemented"
    (TArr _ _ _ ) -> error "genConstraints arr not implemented"
    (TCon _     ) -> error "genConstraints con not implemented"
    (NewType _ _) -> error "genConstraints newtype not implemented"
    (Type _     ) -> error "genConstraints type not implemented"

  go _ = undefined
