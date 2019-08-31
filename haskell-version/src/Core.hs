{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core where

import Control.Arrow hiding (app)
import           Numeric.Natural
import qualified Data.Map.Strict               as Map
import           Data.Functor.Const
import           Data.Functor.Foldable
import           Data.Void
import Control.Comonad.Trans.Cofree (CofreeF(..))

import           Expr
import           Typed

data Core

instance Expression Core where
  type LamOpts Core = ()
  type ExprExt Core f = Typed Core f

instance TypedExpression Core where
  type RigName Core = ()
  type PolName Core = ()
  type ArrowOpts Core = (Abst Rig, Abst Pol, Abst Pol)
  type TypedExt Core f = Const Void

newtype CoreE a
  = MkCoreE
  { unCoreE :: Expr Core (Typed Core (Const Void)) a
  } deriving (Functor)

instance ExprConst Core CoreE where
  injExpr = MkCoreE

instance TypedConst Core CoreE where
  injTyped = MkCoreE . Expr

printCore :: CoreE String -> String
printCore = go . unCoreE
 where
  go = printExpr $ MkPrintExpr
    { printLamOpts   = \_ _ -> "_"
    , printExprInner = printTyped $ MkPrintTyped
      { printRigName    = const ""
      , printPolName    = const ""
      , printArrowOpts  = \(rig, inPol, outPol) input _ -> concat
        [ "["
        , input
        , ", "
        , printAbst printRig rig
        , ", "
        , printAbst printPol inPol
        , ", "
        , printAbst printPol outPol
        , "]"
        ]
      , printTypedInner = const ""
      }
    }

subst :: Fix CoreE -> Fix CoreE -> Fix CoreE
subst thing sub = cata (goExpr . unCoreE) thing (sub, 0) where

  incDepth = second (1+)
   
  goExpr (App x y) = app <$> x <*> y
  goExpr (Lam x body)     = do
    lam x . body . incDepth
  goExpr (Val  (Bound  i))   = \(sub, depth) ->
    if i == depth
      then sub
      else var i
  goExpr (Val  (Inline x))      = x
  goExpr (Val  (Free   name))   = pure $ free name 
  goExpr (Expr x         )      = goType x

  goType (RArr x output) = rig x . output
  goType (PArr x output) = pol x . output
  goType (TArr opts input output) = arrow opts <$> input <*> output
  goType (TCon  name) = pure $ con name
  goType (Typed _   ) = undefined

muToFix :: Functor f => Mu f -> Fix f
muToFix = cata Fix

fixToMu :: Functor f => Fix f -> Mu f
fixToMu = cata (\f -> Mu (\g -> g $ cata g <$> f))

eval :: Fix CoreE -> (Map.Map String (Fix CoreE), [Fix CoreE]) -> Fix CoreE
eval = para (goExpr . unCoreE)
 where

  pushInput x (tbl, xs) = (tbl, x : xs)
  getInput i (_, xs) = if i < length xs
    then xs !! i
    else error $ "Index " <> show i <> " is too big for context " <> show
      (cata printCore <$> xs)
  getFree name (tbl, _) = case Map.lookup name tbl of
    Nothing -> error "Variable was not in context"
    Just x  -> x

  goExpr (App (_, x) (_, y)) = do
    func <- x
    y' <- y
    case unCoreE $ unfix func of
      Lam _ body -> pure $ subst body y'
      _ -> error "Cannot reduce non lambda value!"

  -- Don't evaluate lambdas
  goExpr (Lam x (body, _) )     = pure $ lam x body 

  goExpr (Val  (Bound  i   ))   = getInput (fromIntegral i)
  goExpr (Val  (Inline (_, x))) = x
  goExpr (Val  (Free   name))   = getFree name
  goExpr (Expr x         ) = goType x

  goType (RArr _ (_, output)  ) = rig () . output
  goType (PArr _ (_, output)  ) = pol () . output
  goType (TArr opts (_, input) (_, output)) = do
    input' <- input
    arrow opts input' . output . pushInput input'
  goType (TCon  name) = pure $ con name
  goType (Typed _   ) = undefined
