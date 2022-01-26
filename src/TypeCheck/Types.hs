
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module TypeCheck.Types where

import Control.Applicative
import Data.Foldable (for_)
import AST.LocTree
import Control.Exception
import Control.Monad.Logic
import Control.Monad.State
import Data.IORef
import Data.Functor.Foldable (embed)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Typeable
import qualified Data.Vector as Vec
import Data.Word
import Lexer (RowCol)
import Runtime.Prop
import Runtime.Ref
import Runtime.Term
import Runtime.Types
import Data.HashSet (HashSet)
import Data.Hashable
import qualified Data.HashSet as HS

type ErrorCell = Cell TyCheckM IORef (Maybe TCError)

newtype Branch = Branch { unBranch :: Word32 }
  deriving (Show, Eq, Hashable)

data TyCheckSt = TyCheckSt
  { bindings :: Map Text Binding,
    lambdaDepth :: Word32,
    uidList :: [Word32],
    errors :: [(ErrorCell, Branch)],
    currentBranch :: Branch,
    failedBranches :: HashSet Branch,
    problemDepth :: Word32
  }

data TCError
  = TypeMismatch
  | AmbiguousTypes
  deriving (Show, Eq)

instance Exception TCError where
  toException e = SomeException e
  fromException (SomeException e) = cast e
  displayException TypeMismatch = "Type Mistmatch!"
  displayException AmbiguousTypes = "Ambiguous Types!"

data Binding
  = LambdaBound
      TyCell
    -- | The depth of the lambda from the root, used to
    -- later calculate the De Bruijn indicies
    --
    -- @
    -- \x -> \y -> \z -> ...
    -- @
    --
    -- The values inserted into the bindings map would be
    -- respectively: 0, 1, 2
    --
    -- When the values are looked up, the difference is
    -- taken between the current depth and the depth
    -- of the lambda that bound the value
    --
    -- @
    -- \x -> \f -> f x
    -- @
    --
    -- becomes
    --
    -- @
    -- RtLam (RtLam (RtApp (RtVar 0) (RtVar 1)))
    -- @
      Word32
  | Other TyExpr
newtype TyCheckM a = TyCheckM {runTyCheckM :: LogicT (StateT TyCheckSt IO) a}
  deriving (Functor, Applicative, Monad, Alternative, MonadState TyCheckSt, MonadLogic, MonadIO)

instance Ref TyCheckM IORef where
  newRef = liftIO . newIORef
  readRef = liftIO . readIORef
  writeRef r = liftIO . writeIORef r

data Hole a
  = Filled a
  | Empty
  deriving (Eq)

-- |
-- The Term layer allows for efficient unification
-- The Cell layer gives us propagation and merging
newtype TyCell = TyCell {unTyCell :: Term Word32 IORef (Cell TyCheckM IORef (Hole (RtValF TyCell)))}
  deriving (Eq)

unify :: TyCell -> TyCell -> TyCheckM ()
unify x y = (unTyCell x) `is` (unTyCell y)

newUid :: TyCheckM Word32
newUid = state $ \s -> (Prelude.head $ uidList s, s { uidList = Prelude.tail (uidList s) })

tyCell :: Hole (RtValF TyCell) -> TyCheckM TyCell
tyCell x = do
  uid <- newUid
  TyCell <$> (newTerm uid =<< cell x)

data TCheckEnv = TCheckEnv

data TyExpr = TyExpr {ty :: TyCell, expr :: (RtValF TyExpr)}

makeBaseFunctor ''TyExpr

data Annotated = Annotated {ann :: RtVal, body :: (RtValF Annotated)}

makeBaseFunctor ''Annotated

type TyTree = LocTree RowCol TyExprF

defaultTyCheckSt :: TyCheckSt
defaultTyCheckSt =
  TyCheckSt
    { bindings = Map.empty,
      lambdaDepth = 0,
      uidList = [0..], -- TODO: Do better
      errors = [],
      currentBranch = Branch 0,
      failedBranches = HS.empty,
      problemDepth = 0
    }

-- Must be below the template haskell
instance Lattice TyCheckM (Hole (RtValF TyCell)) where
  -- TYPE
  merge (Old (Filled RtTyF)) (New (Filled RtTyF)) = pure None
  merge (Old (Filled RtTyF)) (New (Filled _)) = pure Conflict
  merge (Old (Filled _)) (New (Filled RtTyF)) = pure Conflict
  -- PRODUCTS
  merge (Old (Filled (RtProdF xs))) (New (Filled (RtProdF ys))) = do
    if Vec.length xs /= Vec.length ys
      then pure Conflict
      else do
        for_ (Vec.zip xs ys) (uncurry unify)
        pure None
  -- ARROWS
  merge (Old (Filled (RtArrF a b))) (New (Filled (RtArrF c d))) = do
    unify a c
    unify b d
    pure None
  merge (Old (Filled x)) (New (Filled y)) = do
    xVal <- liftIO $ embed <$> traverse gatherTy x
    yVal <- liftIO $ embed <$> traverse gatherTy y
    error $ "Haven't fully implemented merge for TyCells yet:\n" <> show xVal <> "\n" <> show yVal
  merge (Old Empty) (New (Filled y)) = pure $ Gain (Filled y)
  merge _ (New Empty) =  pure None

  bottom = pure Empty

  isTop (Filled _) = pure True
  isTop Empty = pure False

treeRootTy :: TyTree -> TyCell
treeRootTy = tyF . locContent

exprToTree :: RowCol -> RowCol -> TyExpr -> TyExprF TyTree
exprToTree start end (TyExpr thisTy val) = TyExprF thisTy $ (LocTree start end . exprToTree start end <$> val)

treeValuesIntoTy :: TyTree -> TyCheckM TyCell
treeValuesIntoTy (LocTree _ _ (TyExprF _ valTree)) = do
  tyCell . Filled =<< traverse treeValuesIntoTy valTree

treeGatherAllTys :: LocTree RowCol TyExprF -> IO (LocTree RowCol AnnotatedF)
treeGatherAllTys (LocTree x y (TyExprF t val)) = do
  val' <- AnnotatedF <$> gatherTy t <*> traverse treeGatherAllTys val
  pure $ LocTree x y val'

treeGatherRootTy :: TyTree -> IO RtVal
treeGatherRootTy tree = gatherTy $ tyF $ locContent tree

gatherTy :: TyCell -> IO RtVal
gatherTy c = evalStateT (go c) (Map.empty, 0)
  where
    go :: TyCell -> StateT (Map Word32 Word32, Word32) IO RtVal
    go (TyCell t) = do
      RootInfo thisCell _ uid _ _ <- liftIO $ rootInfo t
      liftIO (readRef $ value thisCell) >>= \case
        Filled (RtProdF xs) -> RtProd <$> traverse go xs
        Filled (RtArrF input output) -> RtArr <$> go input <*> go output
        Filled RtTyF -> pure RtTy
        Filled _ ->  undefined
        Empty ->
          Map.lookup uid . fst <$> get >>= \case
            Just i -> pure $ RtVar i
            Nothing -> do
              (tbl, n) <- get
              put $ (Map.insert uid n tbl, n + 1)
              pure $ RtVar n

treeStripTypes :: TyTree -> RtVal
treeStripTypes (LocTree _ _ (TyExprF _ e)) =
  embed $ treeStripTypes <$> e
