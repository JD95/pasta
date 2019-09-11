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

import           Control.Monad.Catch.Pure

import           Core.TypeCheck.Check
import           Subst
import           Expr
import           Typed

data UnifyException = CantUnify deriving Show

instance Exception UnifyException

newtype Fill = MkFill { unFill :: String } deriving (Eq)

class (Functor f, Functor g) => Unify f g where
  unify :: (MonadThrow m) => f a -> g a -> m (Either Fill [(a, a)])

instance Unify (Expr Check) (Expr Check) where
  unify (Val (Bound i))(Val (Bound j)) = if i == j then pure . pure $ [] else throwM CantUnify

instance Unify (Typed Check) (Typed Check) where
  unify (RArr _ x)(RArr _ y) = pure . pure $ [(x,y)]
  unify (TArr _ a c)(TArr _ b d) = pure . pure $ [(a,b), (c,d)]
  unify (TCon x)(TCon y) = if x == y
    then pure (Right [])
    else error "Mismatched Types"

instance Unify Checked (Expr Check) where
  unify (Hole s) _ = pure $ Left (MkFill s)

instance Unify Checked (Typed Check) where
  unify (Hole s) _ = pure $ Left (MkFill s)

instance Unify Checked Checked where
  unify (Hole s) _ = pure $ Left (MkFill s)

instance Subst (Expr ix) Fill where
  depth = flip const

  getKey = const Nothing

instance Subst (Typed ix) Fill where
  depth = flip const
  getKey = const Nothing

instance Subst Checked Fill where
  depth _ n = n

  getKey (Hole s) = Just (MkFill s)
