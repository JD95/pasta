{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core where

import           Numeric.Natural
import qualified Data.Map.Strict               as Map
import           Data.Functor.Const
import           Data.Functor.Foldable
import           Data.Void

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

eval :: Mu CoreE -> (Map.Map String (Mu CoreE), [Mu CoreE]) -> Mu CoreE
eval = cata (goExpr . unCoreE)
 where

  pushInput x (tbl, xs) = (tbl, x : xs)
  getInput i (_, xs) = if i < length xs
    then xs !! i
    else error $ "Index " <> show i <> " is too big for context " <> show
      (cata printCore <$> xs)
  getFree name (tbl, _) = case Map.lookup name tbl of
    Nothing -> error "Variable was not in context"
    Just x  -> x

  goExpr (App x y) = do
    y' <- y
    x . pushInput y'
  goExpr (Lam _ body        ) = body
  goExpr (Val  (Bound  i   )) = getInput (fromIntegral i)
  goExpr (Val  (Inline x   )) = x
  goExpr (Val  (Free   name)) = getFree name
  goExpr (Expr x            ) = goType x

  goType (RArr _ output         ) = rig () . output
  goType (PArr _ output         ) = pol () . output
  goType (TArr opts input output) = do
    input' <- input
    arrow opts input' . output . pushInput input'
  goType (TCon  name) = pure $ con name
  goType (Typed _   ) = undefined
