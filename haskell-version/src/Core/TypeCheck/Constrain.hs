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
{-# LANGUAGE TemplateHaskell #-}

module Core.TypeCheck.Constrain where

import           Lens.Micro.Platform
import           Control.Monad.State.Strict
import           Data.Functor.Foldable
import qualified Data.Map.Merge.Strict         as Map
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Numeric.Natural

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

data ConstraintST = ConstraintST
  { _ctx :: Ctx
  , _names :: Names
  }

makeLenses ''ConstraintST

class Monad m => ConstraintGen m where
  require :: W (Fix CheckE) -> m ()
  usage :: String -> Rig -> m ()
  withBinding :: Fix CheckE -> m a -> m a
  lookupBinding :: Natural -> m (Maybe (Fix CheckE))

instance Monad m => ConstraintGen (StateT ConstraintST m) where
  require w = modify $ \st -> st & ctx . constraints %~ ((:) w)
  usage s r = modify $ \st ->
    let f Nothing = Just r
        f (Just r') = Just (r <> r')
    in st & ctx . rigs %~ (Rigs . Map.alter f s . unRigs)
  withBinding x action = do
    stOld <- get
    modify $ \st -> st & ctx . bindings %~ (:) x
    result <- action
    modify $ \st -> st & ctx . bindings .~ (stOld ^. ctx . bindings)
    pure result
  lookupBinding n = do
    st <- get
    pure $ st ^? ctx . bindings . ix (fromIntegral n)

initNames = go (zipWith (\l n -> l : show n) (cycle ['a' .. 'z']) (join $ replicate 26 <$> [1 ..]))
  where go (x : xs) = Next x (go xs)

initConstraintST = ConstraintST mempty initNames

instance Monad m => NameGen (StateT ConstraintST m) where
  newName = do
    st <- get
    let (next, rest) = st ^. names & popName
    put (st & names .~ rest)
    pure next

genConstraints :: (ConstraintGen m, NameGen m) => Fix CoreE -> m (Fix CheckE)
genConstraints = cata go
 where
  go (Here layer) = case layer of

    (Val (Bound i)) -> do
      lookupBinding i >>= \case
        Just term -> pure term
        Nothing   -> error "Binding has no binder!"
    (Val (Free   name)) -> pure $ undefined
    (Val (Inline x   )) -> x

    (Lam _ body       ) -> do
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
      require $ isRig funRig
      require $ isPol funPol
      pure outTy

  go (There (Here layer)) = case layer of
    (RArr _ _  ) -> error "genConstraints rig not implemented"
    (PArr _ _  ) -> error "genConstraints pol not implemented"
    (TArr _ _ _) -> error "genConstraints arr not implemented"
    (TCon _    ) -> error "genConstraints con not implemented"
    (Type _    ) -> error "genConstraints type not implemented"

  go _ = undefined
