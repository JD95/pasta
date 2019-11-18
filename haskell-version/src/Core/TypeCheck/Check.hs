{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.TypeCheck.Check where

import           Data.Functor.Classes
import           Data.Functor.Foldable
import           Data.Proxy
import           Numeric.Natural
import           Data.Foldable
import           Data.Map.Strict                ( Map
                                                , mapWithKey
                                                )

import           Core
import           Display
import           Expr
import           Subst
import           Summable
import           Typed

data Checked a
  = Hole String
  -- ^ Represents a whole in an expression
  | ListH (Map Natural a)
  -- ^ Represents a series of holes in some list
  -- Given that the size of the list is unknown
  -- the indicies are filled and then a concrete
  -- list is checked against it.
  deriving (Functor, Eq)

instance Eq1 Checked where
  liftEq _ (Hole s) (Hole t) = s == t
  liftEq f (ListH xs) (ListH ys) = liftEq f xs ys
  liftEq _ _ _ = False

instance (Display a) => Display (Checked a) where
  display (Hole name) = "?" <> name
  display (ListH mp) =
    let cs = sepBy "," (toList $ mapWithKey (\k v -> show k <> " = " <> display v) mp)
    in "[" <> cs <> "]"

data TypeCheckError

type CheckE = Summed '[ Expr Check, Typed Check, Checked]

instance Display (Expr Check String) where
  display = printExpr $ MkPrintExpr
    { printLamOpts = (const . const) "_"
    , printCaseOpts = error "Display Checked case not implemented"
    }

instance Display (Typed Check String) where
  display = printTyped $ MkPrintTyped
    { printRigName = const ""
    , printPolName = const ""
    , printArrowOpts  = \(rig, inPol) input _ -> concat
      [ "("
      , either (printAbst printRig) (cata display) rig
      , " "
      , either (printAbst printPol) (cata display) inPol
      , " : "
      , input
      , ")"
      ]
    }

data Check

instance Expression Check where
  type LamOpts Check = LamOpts Core
  type CaseOpts Check = CaseOpts Core

instance TypedExpression Check where
  type RigName Check = RigName Core
  type PolName Check = RigName Core
  type ArrowOpts Check = ( Either (Abst Rig) (Fix CheckE)
                         , Either (Abst Pol) (Fix CheckE)
                         )

cke :: Proxy Check
cke = Proxy @Check

hole :: String -> Fix CheckE
hole = Fix . inj . Hole

listH :: Map Natural (Fix CheckE) -> Fix CheckE
listH = Fix . inj . ListH

toCheck :: Fix CoreE -> Fix CheckE
toCheck = cata go
 where
  go (Here layer) = case layer of
    Val (Bound  i) -> mkVar cke i
    Val (Free   x) -> mkFree cke x
    Val (Inline x) -> mkInline cke x
    Nat n -> mkNat cke n
    Str s -> mkStr cke s
    App x y        -> mkApp cke x y
    Lam x body     -> mkLam cke x body
    Record xs      -> mkRec cke xs
    List t xs      -> mkList cke t xs
    Case x xs      -> mkCase cke x xs
    Proj i         -> mkProj cke i
    Inj i x        -> mkInj cke i x
  go (There (Here layer)) = case layer of
    RArr x y        -> mkRig cke x y
    PArr x y        -> mkPol cke x y
    TArr (a, b) x y -> mkArrow cke (Left a, Left b) x y
    TCon x          -> mkCon cke x
    NewType name ty -> mkNewType cke name ty
    Type n          -> mkT cke n
    Ann x y -> mkAnn cke x y
  go _ = undefined

instance Subst Checked Natural where
  depth f n = (,) n <$> f
  getKey = const Nothing
