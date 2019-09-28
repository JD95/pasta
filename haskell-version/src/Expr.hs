{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs, TypeFamilies, RankNTypes, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Expr where

import  Data.List
import           Data.Functor.Foldable
import           Numeric.Natural
import           Data.Proxy

import           Subst
import           Summable

data Expr ix a where
  App :: a -> a -> Expr ix a
  Lam :: LamOpts ix -> a -> Expr ix a
  Val :: Abst a -> Expr ix a
  List :: [a] -> Expr ix a
  Inj :: Natural -> a -> Expr ix a
  Proj :: Natural -> Expr ix a

deriving instance Functor (Expr ix)

class Expression ix where
  type LamOpts ix :: *

data Abst a = Inline a | Bound Natural | Free String deriving (Show, Functor)

data PrintExpr ix
  = MkPrintExpr
  { printLamOpts :: LamOpts ix -> String -> String
  }

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

mkInline
  :: (Injectable (Expr ix) xs) => Proxy ix -> Fix (Summed xs) -> Fix (Summed xs)
mkInline = \(_ :: Proxy ix) x -> Fix . inj . Val @_ @ix $ Inline x

mkList
  :: (Injectable (Expr ix) xs) => Proxy ix -> [Fix (Summed xs)] -> Fix (Summed xs)
mkList = \(_ :: Proxy ix) xs -> Fix . inj $ List @_ @ix xs 

mkInj
  :: (Injectable (Expr ix) xs) => Proxy ix -> Natural -> Fix (Summed xs) -> Fix (Summed xs)
mkInj = \(_ :: Proxy ix) i x -> Fix . inj $ Inj @_ @ix i x 
  
mkProj
  :: (Injectable (Expr ix) xs) => Proxy ix -> Natural -> Fix (Summed xs)
mkProj = \(_ :: Proxy ix) x -> Fix . inj $ Proj @ix x 

printExpr :: PrintExpr ix -> Expr ix String -> String
printExpr (MkPrintExpr lam) = go
 where
  go (App func input) = func <> " " <> input
  go (Lam name body ) = concat ["(\\", lam name body, " -> ", body, ")"]
  go (Val x         ) = printAbst id x
  go (List []) = "[]" 
  go (List [x]) = "[" <> x <> "]" 
  go (List xs) = "[" <> foldl' (\str (x,y) -> str <> x <> ", " <> y) "" (zip xs (tail xs)) <> "]" 
  go (Inj i x) = "Inj " <> show i <> " " <> x
  go (Proj i) = "@ " <> show i

printAbst :: (a -> String) -> Abst a -> String
printAbst f (Inline x) = f x
printAbst _ (Bound  i) = "%" <> show i
printAbst _ (Free   x) = x

instance Subst (Expr ix) Natural where
  depth (Lam a body) n = Lam a (n + 1, body)
  depth f n = (,) n <$> f

  getKey (Val (Bound i)) = Just i
  getKey _ = Nothing
