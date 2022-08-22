{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Runtime.Logic where

import Control.Monad.State
import Data.Kind
import GHC.Word
import Prelude hiding (Bool (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Text (Text)
import Data.Foldable

import Runtime.Ref
import Runtime.Prop

data Part = Term | Op

data Logical (p :: Part) where
  -- Simple Values
  Input :: Logical 'Term
  String :: Text -> Logical 'Term
  -- ADTs
  Index :: Word -> Logical 'Term -> Logical 'Term
  Prod :: Vector (Logical 'Term) -> Logical 'Term
  Tag :: Word -> Logical 'Term -> Logical 'Term
  -- Rels
  App :: Logical 'Op -> Logical 'Term -> Logical 'Op
  Case :: Logical 'Term -> Vector (Logical 'Op) -> Logical 'Op
  Unify :: Logical 'Term -> Logical 'Term -> Logical 'Op
  And :: [Logical 'Op] -> Logical 'Op
  Or :: [Logical 'Op] -> Logical 'Op

deriving instance Eq (Logical p)
deriving instance Show (Logical p)

class LiftTerm t (f :: t -> Type) (p :: t) (q :: t) where
  liftTerm :: f p -> f q

class Monad m => Backtrack m where
  branch :: m a -> m a -> m a
  failBranch :: m a

data RtVal
  = RtString Text
  | RtConstr Word RtVal
  | RtProd (Vector RtVal)

newtype Rt m = Rt { unRt :: Cell m (Ref m) RtVal }

-- Constructs propagator stuff
mkProp :: MonadRef m => Logical 'Term -> m (Rt m)
mkProp Input =
  undefined
mkProp (String s) =
  Rt <$> cell (RtString s)
mkProp (Index i (Prod vals)) =
  mkProp $ vals Vec.! fromIntegral i
mkProp (Index _i _val) = do
  -- setup prop
  undefined
mkProp (Prod vals) = do
  _valsRefs <- traverse mkProp vals
  -- setup propagator
  undefined
mkProp (Tag _i val) = do
  _x <- mkProp val
  -- setup propagator
  undefined

with :: MonadRef m => Rt m -> m () -> m ()
with _ = undefined

withLocal :: MonadRef m => Rt m -> m () -> m ()
withLocal _ = undefined

eval :: (MonadRef m, Backtrack m) => Logical 'Op -> m ()
eval (Or paths) =
  foldr branch failBranch (eval <$> paths)
eval (And steps) =
  for_ steps eval
eval (Unify x y) =
  unify x y
eval (App rel val) = do
  input <- mkProp val
  with input $ eval rel
eval (Case subject _paths) = do
  _s <- mkProp subject
  -- setup prop
  undefined

unify :: Backtrack m => Logical 'Term -> Logical 'Term -> m ()
unify _ _ = undefined

isJust :: Logical 'Op
isJust = And
  [ Case Input $ Vec.fromList
    [ Unify (Index 0 Input) (Tag 0 $ Prod Vec.empty)
    , Unify (Index 0 Input) (Tag 1 $ Prod Vec.empty)
    ]
  ]

edge :: Logical 'Op
edge = Or
  [ And
      [ Unify (Index 0 Input) (String "a")
      , Unify (Index 1 Input) (String "b")
      ]
  , And
      [ Unify (Index 0 Input) (String "b")
      , Unify (Index 1 Input) (String "c")
      ]
  , And
      [ Unify (Index 0 Input) (String "a")
      , Unify (Index 1 Input) (String "d")
      ]
  ]

path :: Logical 'Op
path = Or
  [ App edge Input
  , And
    [ App edge $ Prod $ Vec.fromList [Index 0 Input, Index 2 Input]
    , App path $ Prod $ Vec.fromList [Index 2 Input, Index 1 Input]
    ]
  ]
