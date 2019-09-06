{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Surface where

import           Control.Arrow                  ( first
                                                , second
                                                )
import           Data.Functor.Const
import           Data.Functor.Foldable
import qualified Data.Map.Strict               as Map
import           Data.Monoid
import           Data.Proxy
import           Data.Void
import           Numeric.Natural

import           Core
import           Expr
import           Typed
import           Display
import           Summable

data Surface

instance Expression Surface where
  type LamOpts Surface = String

instance TypedExpression Surface where
  type RigName Surface = String
  type PolName Surface = String
  type ArrowOpts Surface = (String, Abst Rig, Abst Pol, Abst Pol)

type SurfaceE = Summed '[ Expr Surface, Typed Surface ]

instance Display (Expr Surface String) where
  display = printExpr $ MkPrintExpr { printLamOpts   = const }

instance Display (Typed Surface String) where
  display = printTyped $ MkPrintTyped
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
      }

instance ToCore SurfaceE where

  toCore mu = cata (go) mu initCtx
   where

    initCtx = (Map.empty, 0)

    pushName name (tbl, depth) = (Map.insert name depth tbl, depth + 1)

    go (Here layer) =
      let (ExprBuilder app lam var free inline) = exprBuilder (Proxy @Core)
      in  case layer of
            App x    y     -> app <$> x <*> y
            Lam name body  -> lam () . body . pushName name
            Val (Bound  i) -> pure $ var i
            Val (Inline x) -> inline <$> x
            Val (Free   x) -> \(tbl, depth) -> case Map.lookup x tbl of
              Nothing -> free x
              Just n  -> var (depth - n - 1)

    go (There (Here layer)) =
      let (TypeBuilder rig pol arrow con t_) = typeBuilder (Proxy @Core)
      in  case layer of
            RArr name output -> rig () . output . pushName name
            PArr name output -> pol () . output . pushName name
            TArr (name, a, b, c) input output ->
              arrow (a, b, c) <$> input <*> (output . pushName name)
            TCon name -> pure $ con name
            Type n    -> pure $ t_ n

    go _ = undefined
