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

import           Lens.Micro.Platform
import           Data.Functor.Foldable
import qualified Data.Map.Merge.Strict         as Map
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Numeric.Natural
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Control.Exception
import           Control.Monad

import           Constraint
import           Core
import           Env
import           Expr
import           Typed
import           Subst
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

data ConstraintGen m a where
  Require :: W (Fix CheckE) -> ConstraintGen m ()
  Usage :: String -> Rig -> ConstraintGen m ()
  WithBinding :: Fix CheckE -> m a -> ConstraintGen m a
  LookupBinding :: Natural -> ConstraintGen m (Maybe (Fix CheckE))

makeSem ''ConstraintGen

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

initNames = go
  (zipWith (\l n -> l : show n)
           (cycle ['a' .. 'z'])
           (join $ replicate 26 <$> [1 ..])
  )
  where go (x : xs) = Next x (go xs)

initConstraintST = ConstraintST mempty initNames

runNameGenAsState
  :: (Member (State ConstraintST) r) => Sem (NameGen ': r) a -> Sem r a
runNameGenAsState = interpret $ \case
  NewName -> do
    st <- get
    let (next, rest) = st ^. names & popName
    put (st & names .~ rest)
    pure next

genConstraints
  :: (Members '[ConstraintGen, NameGen, Error SomeException] r)
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
      Nothing     -> error "Undefined Symbol"
    (Val (Inline x)) -> x

    (Lam _ body    ) -> do
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
    (RArr _ _  ) -> error "genConstraints rig not implemented"
    (PArr _ _  ) -> error "genConstraints pol not implemented"
    (TArr _ _ _) -> error "genConstraints arr not implemented"
    (TCon _    ) -> error "genConstraints con not implemented"
    (Type _    ) -> error "genConstraints type not implemented"

  go _ = undefined
