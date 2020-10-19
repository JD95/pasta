{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeCheck.Typed.Eval where

import AST.Core
import Control.Monad.Free
import Control.Monad.Trans.Free (FreeF)
import qualified Control.Monad.Trans.Free as F
import Data.Sum
import Data.Text
import TypeCheck.Typed.Stages
import Prelude hiding (pi)

class TySubst f where
  tySubst :: Text -> Partial Hole -> f (Partial Hole, Partial Hole) -> Partial Hole

instance TySubst f => TySubst (FreeF f Hole) where
  tySubst sym val (F.Free new) = tySubst sym val new
  tySubst sym val (F.Pure h) = Pure h

instance Apply TySubst fs => TySubst (Sum fs) where
  tySubst sym val = apply @TySubst (tySubst sym val)

instance TySubst Prim where
  tySubst sym _ (Arr (Just s) (oldIn, newIn) (oldOut, newOut))
    | sym == s = pi s oldIn oldOut
    | otherwise = pi s newIn newOut
  tySubst _ _ (Arr Nothing (_, newIn) (_, newOut)) = newIn -:> newOut
  tySubst _ _ NatTy = natTy
  tySubst _ _ IntTy = intTy
  tySubst _ _ (Type i) = ty i

instance TySubst Data where
  tySubst = undefined

instance TySubst App where
  tySubst = undefined

instance TySubst Lam where
  tySubst sym val (Lam x (old, new))
    | sym == x = lam x old
    | otherwise = lam x new

instance TySubst FreeVar where
  tySubst sym val (FreeVar x)
    | sym == x = val
    | otherwise = free x

instance TySubst Ann where
  tySubst = undefined
