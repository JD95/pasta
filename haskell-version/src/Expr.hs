{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Expr where

import           Data.Functor.Classes
import           Data.Functor.Foldable   hiding ( fold )
import           Data.Foldable
import           Numeric.Natural
import           Data.Proxy
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Map.Merge.Strict
import           Data.Monoid                    ( All(..) )

import           Display
import           Subst
import           Summable

data ListType = Sum | Prod deriving (Show, Eq)

data Lookup a = Index Natural | Key String | Unique a deriving (Show, Eq, Functor, Foldable, Traversable)

instance Eq1 Lookup where
  liftEq _ (Index a) (Index b) = a == b
  liftEq _ (Key a) (Key b) = a == b
  liftEq f (Unique a) (Unique b) = f a b
  liftEq _ _ _ = False

data Expr ix a where
  App :: a -> a -> Expr ix a
  Lam :: LamOpts ix -> a -> Expr ix a
  Nat :: Natural -> Expr ix a
  Str :: String -> Expr ix a
  Val :: Abst a -> Expr ix a
  List :: ListType -> [a] -> Expr ix a
  Record :: Map String a -> Expr ix a
  Inj :: Lookup a -> a -> Expr ix a
  Proj :: Lookup a -> Expr ix a
  Case :: a -> [(Natural, CaseOpts ix, a)] -> Expr ix a

deriving instance Functor (Expr ix)

instance (Eq (CaseOpts ix), Eq (LamOpts ix)) => Eq1 (Expr ix) where
  liftEq f (App x y) (App x' y') = and [f x x', f y y']
  liftEq f (Lam opt x) (Lam opt' x') = and [opt == opt', f x x']
  liftEq f (Val x) (Val y) = liftEq f x y
  liftEq f (List t xs) (List t' ys) = and [t == t', liftEq f xs ys]
  liftEq f (Record x) (Record y) =
    getAll . fold . fmap All $
    merge (mapMissing (\_ _ -> False)) (mapMissing (\_ _ -> False)) (zipWithMatched (const f)) x y
  liftEq f (Inj n x) (Inj m y) = liftEq f n m && f x y
  liftEq f (Proj n) (Proj m) = liftEq f n m
  liftEq f (Case x xs) (Case x' xs') =
    let cmp (n,opt,a) (n',opt',b) = and
          [n == n', opt == opt', f a b]
    in f x x' && liftEq cmp xs xs'
  liftEq _ _ _ = False

class Expression ix where
  type LamOpts ix :: *
  type CaseOpts ix :: *

caseLookupIndex
  :: Proxy ix
  -> Natural
  -> [(Natural, CaseOpts ix, a)]
  -> Maybe (CaseOpts ix, a)
caseLookupIndex _ _ [] = Nothing
caseLookupIndex p n ((m, c, v) : xs) =
  if n == m then Just (c, v) else caseLookupIndex p n xs

data Abst a = Inline a | Bound Natural | Free String deriving (Show, Eq, Functor)

instance Eq1 Abst where
  liftEq f (Inline a) (Inline b) = f a b
  liftEq _ (Bound n) (Bound m) = n == m
  liftEq _ (Free n) (Free m) = n == m
  liftEq _ _ _ = False

mkApp
  :: (Injectable (Expr ix) xs)
  => Proxy ix
  -> Fix (Summed xs)
  -> Fix (Summed xs)
  -> Fix (Summed xs)
mkApp = \(_ :: Proxy ix) func input -> Fix . inj $ App @_ @ix func input

mkApp'
  :: (Injectable (Expr ix) xs)
  => Proxy ix
  -> Fix (Summed xs)
  -> [Fix (Summed xs)]
  -> Fix (Summed xs)
mkApp' (_ :: Proxy ix) func [] = func
mkApp' (p :: Proxy ix) func (x : xs) =
  mkApp' p (Fix . inj $ App @_ @ix func x) xs

mkLam
  :: (Injectable (Expr ix) xs)
  => Proxy ix
  -> LamOpts ix
  -> Fix (Summed xs)
  -> Fix (Summed xs)
mkLam = \(_ :: Proxy ix) opts body -> Fix . inj $ Lam @ix opts body

mkVar :: (Injectable (Expr ix) xs) => Proxy ix -> Natural -> Fix (Summed xs)
mkVar = \(_ :: Proxy ix) i -> Fix . inj . Val @_ @ix $ Bound i

mkFree :: (Injectable (Expr ix) xs) => Proxy ix -> String -> Fix (Summed xs)
mkFree = \(_ :: Proxy ix) name -> Fix . inj . Val @_ @ix $ Free name

mkNat :: (Injectable (Expr ix) xs) => Proxy ix -> Natural -> Fix (Summed xs)
mkNat = \(_ :: Proxy ix) val -> Fix . inj . Nat @ix $ val 

mkStr :: (Injectable (Expr ix) xs) => Proxy ix -> String -> Fix (Summed xs)
mkStr = \(_ :: Proxy ix) val -> Fix . inj . Str @ix $ val 

mkInline
  :: (Injectable (Expr ix) xs) => Proxy ix -> Fix (Summed xs) -> Fix (Summed xs)
mkInline = \(_ :: Proxy ix) x -> Fix . inj . Val @_ @ix $ Inline x

mkList
  :: (Injectable (Expr ix) xs)
  => Proxy ix
  -> ListType
  -> [Fix (Summed xs)]
  -> Fix (Summed xs)
mkList = \(_ :: Proxy ix) t xs -> Fix . inj $ List @_ @ix t xs

mkInj
  :: (Injectable (Expr ix) xs)
  => Proxy ix
  -> Lookup (Fix (Summed xs))
  -> Fix (Summed xs)
  -> Fix (Summed xs)
mkInj = \(_ :: Proxy ix) i x -> Fix . inj $ Inj @_ @ix i x

mkProj
  :: (Injectable (Expr ix) xs)
  => Proxy ix
  -> Lookup (Fix (Summed xs))
  -> Fix (Summed xs)
mkProj = \(_ :: Proxy ix) x -> Fix . inj $ Proj @_ @ix x

mkRec
  :: (Injectable (Expr ix) xs)
  => Proxy ix
  -> Map String (Fix (Summed xs))
  -> Fix (Summed xs)
mkRec = \(_ :: Proxy ix) xs -> Fix . inj $ Record @_ @ix xs

mkRec'
  :: (Injectable (Expr ix) xs)
  => Proxy ix
  -> [(String, Fix (Summed xs))]
  -> Fix (Summed xs)
mkRec' = \ix xs -> mkRec ix (Map.fromList xs)

mkCase
  :: (Injectable (Expr ix) xs)
  => Proxy ix
  -> Fix (Summed xs)
  -> [(Natural, CaseOpts ix, Fix (Summed xs))]
  -> Fix (Summed xs)
mkCase = \(_ :: Proxy ix) x xs -> Fix . inj $ Case @_ @ix x xs

data PrintExpr ix
  = MkPrintExpr
  { printLamOpts :: LamOpts ix -> String -> String
  , printCaseOpts :: CaseOpts ix -> String
  }

printExpr :: PrintExpr ix -> Expr ix String -> String
printExpr (MkPrintExpr lam cse) = go
 where
  go (App func input) = func <> " " <> input
  go (Lam name body ) = concat ["(\\", lam name body, " -> ", body, ")"]
  go (Val x         ) = printAbst id x
  go (Nat n) = show n
  go (Str  s) = show s
  go (Record x) =
    "{" <> (sepBy "; " . fmap (\(k, v) -> k <> ": " <> v) $ Map.toList x) <> "}"
  go (List Sum  xs) = "(" <> sepBy " | " xs <> ")"
  go (List Prod xs) = "(" <> sepBy ", " xs <> ")"
  go (Inj  i    x ) = "Inj " <> show i <> " " <> x
  go (Proj i      ) = "@ " <> show i
  go (Case x xs) =
    let printCase (n, v, c) = "Inj " <> show n <> " " <> cse v <> " -> " <> c
    in  "case " <> x <> " of { " <> sepBy "; " (printCase <$> xs) <> "} "

printAbst :: (a -> String) -> Abst a -> String
printAbst f (Inline x) = f x
printAbst _ (Bound  i) = "%" <> show i
printAbst _ (Free   x) = x

instance Subst (Expr ix) Natural where
  depth (Lam a body) n = Lam a (n + 1, body)
  depth f n = (,) n <$> f

  getKey (Val (Bound i)) = Just i
  getKey _ = Nothing
