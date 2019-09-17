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

module Core.TypeCheck.Unify where

import           Control.Exception              ( Exception )
import           Polysemy
import           Polysemy.Error

import           Core.TypeCheck.Check
import           Display
import           Env
import           Expr
import           Subst
import           Typed

data UnifyException = CantUnify deriving Show

instance Exception UnifyException

data Fill = MkFill { unFill :: String } deriving (Eq, Ord)

data SubUnify  = SubEq | SubArr

data SubTerm a = SubTerm SubUnify a a deriving Functor

instance Display (SubTerm String) where
  display (SubTerm _ x y) = x <> " ~ " <> y

class (Functor f, Functor g) => Unify f g where
  unify :: (Members '[NameGen, Error UnifyException] r) => f a -> g a -> Sem r (Either Fill [SubTerm a])

instance Unify (Expr Check) (Expr Check) where
  unify (Val (Bound i))(Val (Bound j)) =
    if i == j then pure . pure $ [] else throw CantUnify
  unify _ _ = throw CantUnify

instance Unify (Typed Check) (Typed Check) where
  unify (RArr _ x)(RArr _ y) = pure . pure $ [SubTerm SubEq x y]
  unify (TArr _ a c)(TArr _ b d) = pure . pure $ [SubTerm SubEq a b, SubTerm SubArr c d]
  unify (TCon x)(TCon y) = if x == y
    then pure (Right [])
    else throw CantUnify
  unify _ _ = throw CantUnify

instance Unify Checked (Expr Check) where
  unify (Hole s) _ = pure $ Left (MkFill s)

instance Unify Checked (Typed Check) where
  unify (Hole s) _ = pure $ Left (MkFill s)

instance Unify Checked Checked where
  unify (Hole s) _ = pure $ Left (MkFill s)

instance Subst (Expr ix) Fill where
  depth f n = (,) n <$> f

  getKey = const Nothing

instance Subst (Typed ix) Fill where
  depth f n = (,) n <$> f
  getKey = const Nothing

instance Subst Checked Fill where
  depth f n = (,) n <$> f

  getKey (Hole s) = Just (MkFill s)
