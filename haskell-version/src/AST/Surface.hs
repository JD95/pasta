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
import Data.Eq.Deriving
import Data.Sum
import Parser.Lexer
import RIO hiding (Data)
import Text.Show.Deriving

type SurfComps = Sum '[Prim, Data, Lam, App, FreeVar]

data Surface a = Parent (SurfComps a) (Row, Col) (Row, Col) | Node (SurfComps a) Row Col
  deriving (Eq, Show, Functor, Foldable, Traversable)

deriveShow1 ''Surface
deriveEq1 ''Surface

start :: Surface a -> (Row, Col)
start (Parent _ x _) = x
start (Node _ r c) = (r, c)

end :: Surface a -> (Row, Col)
end (Parent _ _ y) = y
end (Node _ r c) = (r, c)
