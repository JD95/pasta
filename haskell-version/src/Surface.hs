{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Surface where

import           Data.Functor.Foldable
import qualified Data.Map.Strict               as Map
import           Data.Proxy

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
  type ArrowOpts Surface = (String, Abst Rig, Abst Pol)

type SurfaceE = Summed '[ Expr Surface, Typed Surface ]

instance Display (Expr Surface String) where
  display = printExpr $ MkPrintExpr { printLamOpts   = const }

instance Display (Typed Surface String) where
  display = printTyped $ MkPrintTyped
      { printRigName    = id
      , printPolName    = id
      , printArrowOpts  = \(name, rig, inPol) input _ -> concat
        [ "("
        , printAbst printRig rig
        , printAbst printPol inPol
        , " "
        , name
        , " : "
        , input
        , ")"
        ]
      }

instance ToCore SurfaceE where

  toCore mu = cata (go) mu initCtx
   where

    initCtx = (Map.empty, 0)

    pushName name (tbl, depth) = (Map.insert name depth tbl, depth + 1)

    go (Here layer) =
      case layer of
            App x    y     -> mkApp ce <$> x <*> y
            Lam name body  -> mkLam ce  () . body . pushName name
            Val (Bound  i) -> pure $ mkVar ce i
            Val (Inline x) -> mkInline ce <$> x
            Val (Free   x) -> \(tbl, depth) -> case Map.lookup x tbl of
              Nothing -> mkFree ce x
              Just n  -> mkVar ce (depth - n - 1)

    go (There (Here layer)) =
      case layer of
            RArr name output -> mkRig ce () . output . pushName name
            PArr name output -> mkPol ce () . output . pushName name
            TArr (name, a, b) input output ->
              mkArrow ce (a, b) <$> input <*> (output . pushName name)
            TCon name -> pure $ mkCon ce name
            Type n    -> pure $ mkT ce n

    go _ = undefined

se :: Proxy Surface
se = Proxy @Surface
