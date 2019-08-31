{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Surface where

import qualified Data.Map.Strict               as Map
import           Data.Functor.Foldable
import           Data.Functor.Const
import           Data.Void
import           Data.Monoid
import           Numeric.Natural
import           Control.Arrow                  ( first
                                                , second
                                                )

import           Core
import           Expr
import           Typed

data Surface

instance Expression Surface where
  type LamOpts Surface = String
  type ExprExt Surface f = Typed Surface f

instance TypedExpression Surface where
  type RigName Surface = String
  type PolName Surface = String
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
      { printRigName    = id
      , printPolName    = id
      , printArrowOpts  = \(name, rig, inPol, outPol) input _ -> concat
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

toCore :: Fix SurfaceE -> Fix CoreE
toCore mu = cata (goExpr . unSurfaceE) mu initCtx
 where

  initCtx = (Map.empty, 0)

  goExpr (App x    y    ) = app <$> x <*> y
  goExpr (Lam name body ) = lam () . body . pushName name
  goExpr (Val (Bound  i)) = pure $ var i
  goExpr (Val (Inline x)) = inline <$> x
  goExpr (Val (Free   x)) = \(tbl, depth) -> case Map.lookup x tbl of
    Nothing -> free x
    Just n  -> var (depth - n - 1)
  goExpr (Expr x) = goType x

  goType (RArr name output) = rig () . output . pushName name
  goType (PArr name output) = pol () . output . pushName name
  goType (TArr (name, a, b, c) input output) =
    arrow (a, b, c) <$> input <*> (output . pushName name)
  goType (TCon  name) = pure $ con name
  goType (Typed _   ) = undefined

  pushName name (tbl, depth) = (Map.insert name depth tbl, depth + 1)
