{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeCheck.Typed.Eval where

import AST.Core
import AST.Transform
import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans.Free (FreeF)
import qualified Control.Monad.Trans.Free as F
import Data.Functor.Foldable (Fix (..), cata)
import Data.Sum
import Data.Text
import Eval.Flatten
import Eval.Normal
import Eval.Stages
import Eval.WHNF
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

class ToTerm f where
  toTermF :: f (IO (Maybe (Fix Term))) -> IO (Maybe (Fix Term))

instance Apply ToTerm fs => ToTerm (Sum fs) where
  toTermF = apply @ToTerm toTermF

instance ToTerm f => ToTerm (FreeF f Hole) where
  toTermF (F.Free x) = toTermF x
  toTermF (F.Pure h) = pure Nothing

instance ToTerm Prim where
  toTermF = (fmap . fmap) Fix . fmap sequence . sequence . Term . inject

instance ToTerm Data where
  toTermF = (fmap . fmap) Fix . fmap sequence . sequence . Term . inject

instance ToTerm App where
  toTermF (App getFunc getInput) = do
    func' <- getFunc
    input' <- getInput
    pure $ do
      func <- func'
      input <- input'
      pure . Fix . Term . inject . Thunk [input] $ func

instance ToTerm Lam where
  toTermF (Lam x body) = do
    error "Need to add x to bound vars"
    body

instance ToTerm FreeVar where
  toTermF (FreeVar x) = do
    error "Need to lookup x from bound vars"

instance ToTerm Ann where
  toTermF (Ann x _) = x

toTerm :: Partial Hole -> IO (Maybe (Fix Term))
toTerm = cata toTermF

tyEval :: Fix Term -> IO (Fix Flat)
tyEval = runFlatten <=< runNormal []

class FromFlat f where
  fromFlatF :: f (Partial Hole) -> Partial Hole

instance Apply FromFlat fs => FromFlat (Sum fs) where
  fromFlatF = apply @FromFlat fromFlatF

instance FromFlat Prim where
  fromFlatF = Free . inject

instance FromFlat Data where
  fromFlatF = Free . inject

fromFlat :: Fix Flat -> Partial Hole
fromFlat = cata fromFlatF
