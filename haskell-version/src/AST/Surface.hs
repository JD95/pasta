{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AST.Surface where

import AST.Core
import AST.Core.Data
import AST.Core.Prim
import AST.Transform
import Control.Monad.Free
import Data.Functor.Foldable
import Data.Sum
import Parser.Lexer
import RIO hiding (Data)
import TypeCheck.Typed.Stages

type SurfComps = Sum '[Prim, Data, Lam, App, FreeVar, Ann]

type Surface = Tagged SurfComps PosInfo

surface :: SurfComps (Fix Surface) -> PosInfo -> Fix Surface
surface a e = Fix $ Tagged a e

class Desugar f where
  desugarF :: e -> f (Fix (Tagged Typed e)) -> Fix (Tagged Typed e)

instance Desugar Prim where
  desugarF e p = Fix $ Tagged (inject p) e

instance Desugar Data where
  desugarF e p = Fix $ Tagged (inject p) e

instance Desugar Lam where
  desugarF e p = Fix $ Tagged (inject p) e

instance Desugar App where
  desugarF e p = Fix $ Tagged (inject p) e

instance Desugar FreeVar where
  desugarF e p = Fix $ Tagged (inject p) e

instance Desugar Ann where
  desugarF e p = Fix $ Tagged (inject p) e
