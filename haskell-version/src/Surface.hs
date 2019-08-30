{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Surface where

import           Data.Functor.Foldable
import           Data.Functor.Const
import           Data.Void

import           Expr
import           Typed

data Surface

instance Expression Surface where
  type LamOpts Surface = String
  type ExprExt Surface f = Typed Surface f

instance TypedExpression Surface where
  type ArrowOpts Surface = (String, Abst Rig, Abst Pol, Abst Pol)
  type TypedExt Surface f = Const Void

newtype SurfaceE a
  = MkSurfaceE
  { unSurfaceE :: Expr Surface (Typed Surface (Const Void)) a
  } deriving (Functor)

instance ExprConst Surface SurfaceE where
  injExpr = MkSurfaceE

instance TypedConst Surface SurfaceE where
  injTyped = MkSurfaceE . Expr

printSurface :: SurfaceE String -> String
printSurface = go . unSurfaceE
 where
  go = printExpr $ MkPrintExpr
    { printLamOpts   = const
    , printExprInner = printTyped $ MkPrintTyped
      { printArrowOpts  = \(name, rig, inPol, outPol) input _ -> concat
        [ "["
        , name
        , " : "
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
