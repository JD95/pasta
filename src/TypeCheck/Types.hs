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
import Data.Functor.Foldable (cata, embed)
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

newtype Branch = Branch { unBranch :: Word32 }
  deriving (Show, Eq, Hashable)

-- | The context for type checking
data TyCheckSt = TyCheckSt
  { -- | The current bindings for
    -- - Free Variables
    -- - Lambda Expressions
    -- - Let Expressions
    bindings :: Map Text Binding,
    -- | How many lambdas have
    -- been passed up until this
    -- point.
    lambdaDepth :: Word32,
    -- | UIDs for type terms
    uidList :: [Word32],
    -- | The errors accumulated and to which
    -- branch they belong
    errors :: [(TCError, Branch)],
    currentBranch :: Branch,
    failedBranches :: HashSet Branch,
    -- | Used for indentation when
    -- printing debug messages
    problemDepth :: Word32
  }

data TCError
  = TypeMismatch
  | AmbiguousTypes
      -- | The duplicate branch
      -- that caused the ambiguity
      Branch
  deriving (Show, Eq)

instance Exception TCError where
  toException e = SomeException e
  fromException (SomeException e) = cast e
  displayException TypeMismatch = "Type Mistmatch!"
  displayException (AmbiguousTypes _) = "Ambiguous Types!"

data Binding
  = LambdaBound
      TyTerm
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

-- | The monad in which type checking occurs
--
-- It has three layers
-- - LogicT
-- - StateT
-- - IO
--
-- The resulting values will be IO ([a], TyCheckSt)
--
-- The LogicT layer is used for control flow and backtracking, which
-- is necessary for the propagators that power unification. However,
-- the type checking context remains *global* throughout the entire
-- process. This is so errors from different branches can be held
-- onto.
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
newtype TyTerm = TyTerm {unTyTerm :: Term Word32 IORef (Cell TyCheckM IORef (Hole (RtValF TyTerm)))}
  deriving (Eq)

-- | Unify two terms, ensuring that the information
-- within each is compatible. If the information
-- creates a conflict, then backtracking will be
-- caused
unify :: TyTerm -> TyTerm -> TyCheckM ()
unify x y = (unTyTerm x) `is` (unTyTerm y)

newUid :: TyCheckM Word32
newUid = state $ \s -> (Prelude.head $ uidList s, s { uidList = Prelude.tail (uidList s) })

-- | Create a new type term
tyTerm :: Hole (RtValF TyTerm) -> TyCheckM TyTerm
tyTerm x = do
  uid <- newUid
  TyTerm <$> (newTerm uid =<< cell x)

data TCheckEnv = TCheckEnv

-- | A value with each node annotated by a type cell
data TyExpr = TyExpr {ty :: TyTerm, expr :: (RtValF TyExpr)}

makeBaseFunctor ''TyExpr

-- | A value with each node annotated by a type value
data Gathered = Gathered {ann :: RtVal, body :: (RtValF Gathered)}

makeBaseFunctor ''Gathered

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
instance Lattice TyCheckM (Hole (RtValF TyTerm)) where
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
    error $ "Haven't fully implemented merge for TyTerms yet:\n" <> show xVal <> "\n" <> show yVal
  merge (Old Empty) (New (Filled y)) = pure $ Gain (Filled y)
  merge _ (New Empty) =  pure None

  bottom = pure Empty

  isTop (Filled _) = pure True
  isTop Empty = pure False

-- | Get the annotation of the tree's root
treeRootTy :: TyTree -> TyTerm
treeRootTy = tyF . locContent

-- | Wrap each layer of the annotated expression with location info
--
-- The whole expression will lie within
-- provided start and end positions.
exprToTree :: RowCol -> RowCol -> TyExpr -> TyExprF TyTree
exprToTree start end (TyExpr thisTy val) = TyExprF thisTy $ (LocTree start end . exprToTree start end <$> val)

-- | Strips the annotations from the tree and converts
-- the resulting value into a term
treeValuesIntoTy :: TyTree -> TyCheckM TyTerm
treeValuesIntoTy (LocTree _ _ (TyExprF _ valTree)) = do
  tyTerm . Filled =<< traverse treeValuesIntoTy valTree

-- | Gather the annotated type terms into values along the tree
treeGatherAllTys :: LocTree RowCol TyExprF -> IO (LocTree RowCol GatheredF)
treeGatherAllTys (LocTree x y (TyExprF t val)) = do
  val' <- GatheredF <$> gatherTy t <*> traverse treeGatherAllTys val
  pure $ LocTree x y val'

-- | Gather the annotated type for the root node into a value
treeGatherRootTy :: TyTree -> IO RtVal
treeGatherRootTy tree = gatherTy $ tyF $ locContent tree

-- | Gather the term into a value such that all
-- constructors point directly to their children
-- instead of through a term reference
gatherTy :: TyTerm -> IO RtVal
gatherTy c = evalStateT (go c) (Map.empty, 0)
  where
    go :: TyTerm -> StateT (Map Word32 Word32, Word32) IO RtVal
    go (TyTerm t) = do
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

disperseTy :: RtVal -> TyCheckM TyTerm
disperseTy = cata (tyTerm . Filled <=< sequence)

-- | Strip the type term annotations from the tree
treeStripTypes :: TyTree -> RtVal
treeStripTypes (LocTree _ _ (TyExprF _ e)) =
  embed $ treeStripTypes <$> e
