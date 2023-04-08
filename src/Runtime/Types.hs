{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Runtime.Types where

-- import Data.List

-- import qualified Data.Vector as Vec

import Control.Applicative
import Control.Monad.State
import Data.Coerce
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.IORef
import Data.List.NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import Data.Void
import Data.Word
import Runtime.Logic.Partial
import Runtime.Prop
import Runtime.Ref
import System.Mem.StableName
import Prelude hiding (const, log)

newtype ConstF f b a = ConstF {unConstF :: f b}

data RtVal r
  = RtArr (Maybe r) (RtVal r) (RtVal r)
  | RtTy
  | RtLam (Maybe r) (RtVal r)
  | RtVar r
  deriving (Show, Eq)

makeBaseFunctor ''RtVal

deriving instance (Eq r, Eq a) => Eq (RtValF r a)

instance EqH (ConstF (RtValF Void)) where
  eqH (ConstF x) (ConstF y) = x == y

instance MergeableH (ConstF (RtValF Void)) where
  mergeH (Old (ConstF old)) (New (ConstF new)) =
    case (old, new) of
      (RtArrF oldName oldTy oldOut, RtArrF newName newTy newOut) ->
        let name = sequenceA $ merge <$> (Old <$> oldName) <*> (New <$> newName)
            ty = merge (Old oldTy) (New newTy)
            out = merge (Old oldOut) (New newOut)
         in coerce $ RtArrF <$> name <*> ty <*> out

instance TopH (ConstF (RtValF Void)) where
  isTopH = f . unConstF
    where
      f RtTyF = True
      f (RtArrF _ x y) = isTop x && isTop y
      f (RtLamF _ x) = isTop x
      f (RtVarF x) = absurd x

data Expr a
  = Lam a a
  | Case a [Expr]
  | App a a a

rtIdTy :: RtVal String
rtIdTy = RtArr (Just "a") RtTy (RtArr Nothing (RtVar "a") (RtVar "a"))

rtId :: RtVal String
rtId = RtLam Nothing (RtLam (Just "x") (RtVar "x"))

newtype Term = Term {unTerm :: Cell StableName IO IORef (Partial (ConstF (RtValF Void)) ())}

{-
readTerm = readCell . unTerm

newtype TyM a = TyM a

type PartialTerm = RtValF Void (Partial (ConstF (RtValF Void)) ())

newTerm :: (MonadIO m) => m Term
newTerm = liftIO . fmap Term . cell $ Partial Nothing

term :: (MonadIO m) => PartialTerm -> m Term
term = liftIO . fmap Term . cell . Partial . Just . ConstF

learn :: (MonadIO m) => Term -> PartialTerm -> m ()
learn x = liftIO . inform (unTerm x) . Partial . Just . ConstF

realize :: MonadIO m => RtVal String -> m (RtVal Term)
realize ast = evalStateT (cata f ast) Map.empty
  where
    f :: MonadIO m => RtValF String (m (RtVal Term)) -> m (RtVal Term)
    f (RtArrF inputStr mkInputTy mkOutput) = do
      inputTy <- mkInputTy
      input <- newTerm
      -- insert input into context
      output <- mkOutput
      pure $ RtArr (Just input) inputTy output

data Ctx = Ctx {ctxStack :: Map String (RtVal Term)}

infer :: (Alternative m, MonadIO m) => RtVal Term -> StateT Ctx m Term
infer = cata f
  where
    f :: (Alternative m, MonadIO m) => RtValF Term (StateT Ctx m Term) -> StateT Ctx m Term
    f (RtArrF input mkInputTy mkOutput) = do
      inputTy <- mkInputTy
      learn inputTy RtTyF
      output <- mkOutput
      learn output RtTyF
      term RtTyF

synthesize :: (MonadIO m, Alternative m) => RtVal Term -> StateT Ctx m Term
synthesize = cata f
  where
    f :: MonadIO m => RtValF Term (StateT Ctx m Term) -> StateT Ctx m Term
    f (RtArrF _ _ mkBody) = do
      result <- newTerm
      body <- mkBody
      liftIO $
        prop [Watched (unTerm body)] (unTerm result) $ do
          b <- readTerm body
          pure $ Partial . Just . ConstF $ RtLamF Nothing _
      pure result

{-
unit :: RtVal
unit = RtProd Vec.empty

unitF :: RtValF a
unitF = RtProdF Vec.empty

displayRtVal :: RtVal -> String
displayRtVal = cata $ \case
  RtArrF input output -> parens $ input <> " -> " <> output
  RtAppF func ins -> intercalate " " $ func : Vec.toList ins
  RtProdF xs -> parens $ intercalate ", " (Vec.toList xs)
  RtVarF i -> "$" <> show i
  RtLamF body -> parens $ "\\ -> " <> body
  RtTyF -> "Type"
  RtDepTyF i -> "#" <> show i
  RtPrimF _ -> undefined
  RtUnknownF i -> "?" <> show i
  RtConF _ _ -> undefined
  RtAmbiguousF xs -> "{" <> intercalate ", " (toList xs) <> "}"
  where
    parens x = "(" <> x <> ")"
-}
-}
