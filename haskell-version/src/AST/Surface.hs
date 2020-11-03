{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AST.Surface where

import AST.Core
import AST.Core.Data
import AST.Core.Prim
import AST.Transform
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Functor.Foldable
import Data.Sum
import Parser.Lexer
import RIO hiding (Data)
import TypeCheck.Typed.Stages

type SurfComps = Sum '[Prim, Data, Lam, App, FreeVar, Ann]

type Surface = Cofree SurfComps PosInfo

surface :: SurfComps Surface -> PosInfo -> Surface
surface a e = e :< a

class Desugar f where
  desugarF :: f a -> Typed a

instance Apply Desugar fs => Desugar (Sum fs) where
  desugarF = apply @Desugar desugarF

instance Desugar Prim where
  desugarF = inject

instance Desugar Data where
  desugarF = inject

instance Desugar Lam where
  desugarF = inject

instance Desugar App where
  desugarF = inject

instance Desugar FreeVar where
  desugarF = inject

instance Desugar Ann where
  desugarF = inject

desugar :: (Functor f, Desugar f) => Cofree f a -> Cofree Typed a
desugar (x :< fs) = x :< desugarF (desugar <$> fs)
