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

import Data.Sequence (Seq)
import qualified Data.Text as Text
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import Data.Foldable (toList, for_)
import AST.LocTree hiding (foldM)
import Control.Exception
import Control.Monad.Logic
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty(..))
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

data CollectedEither b a
  = One a
  | Many (NonEmpty b)
  deriving (Functor)

instance Applicative (CollectedEither b) where
  pure x = One x

  One f <*> One x = One $ f x
  One _ <*> Many xs = Many xs
  Many xs <*> One _ = Many xs
  Many xs <*> Many ys = Many $ xs <> ys

instance Monad (CollectedEither b) where
  One x >>= f = f x
  Many xs >>= _ = Many xs

instance Foldable (CollectedEither b) where
  foldMap f (One x) = f x
  foldMap _ _ = mempty

instance Traversable (CollectedEither b) where
  traverse f (One x) = One <$> f x
  traverse _ (Many x) = pure $ Many x

instance (Eq a, Eq b) => Eq (CollectedEither b a) where
  One x == One y = x == y
  Many xs == Many ys = xs == ys
  _ == _ = False

newtype Branch = Branch { unBranch :: Word32 }
  deriving (Show, Eq, Hashable)

-- | The context for type checking
data TyCheckSt = TyCheckSt
  { -- | The current bindings for
    -- - Free Variables
    -- - Lambda Expressions
    -- - Let Expressions
    bindings :: Map Text Binding,
    -- | Bound Values
    depTyStack :: Seq TyTerm,
    depTyDepth :: Word32,
    lamStack :: Seq TyTerm,
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
  = TypeMismatch RowCol RowCol RtVal RtVal
  | AmbiguousTypes
      -- | The duplicate branch
      -- that caused the ambiguity
      Branch
  deriving (Show, Eq)

instance Exception TCError where
  toException e = SomeException e
  fromException (SomeException e) = cast e
  displayException (TypeMismatch start end actual expected)
    = concat @[]
    [ "Type Mistmatch at ",
      show start,
      "-",
      show end,
      ":\nexpected '",
      show expected,
      "'\nactual '",
      show actual,
      "'"
    ]

  displayException (AmbiguousTypes _) = "Ambiguous Types!"

data Binding
  = DepTyBound
      TyTerm
      Word32
  | LambdaBound
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
  | Ambiguous (NonEmpty a)
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
      depTyStack = [],
      depTyDepth = 0,
      lamStack = [],
      lambdaDepth = 0,
      uidList = [0..], -- TODO: Do better
      errors = [],
      currentBranch = Branch 0,
      failedBranches = HS.empty,
      problemDepth = 0
    }

-- In this case ambiguous information is being
-- applied to some information
--
-- Type <~ {(a,b), Type}
--
-- The above would result in no information gain
-- because although Type is in conflict with (a,b)
-- in the ambiguous set, is not in conflict with
-- the `Type` value.
--
-- Type <~ {(a,b), () -> ()}
--
-- This however, would be in conflict
mergeAmb :: Old (Hole (RtValF TyTerm)) -> Info (Hole (RtValF TyTerm)) -> RtValF TyTerm -> TyCheckM (Info (Hole (RtValF TyTerm)))
mergeAmb x (Gain z) y = do
  liftIO $ putStrLn "gain"
  ifte
    (merge x (New (Filled y)))
    (\case
        -- If more information has been gained
        -- it shouldd be compatible with the previous
        -- information gained
        Gain y' -> do
          liftIO $ putStrLn "gain2"
          ifte
            (merge (Old z) (New y'))
            pure
            -- If they are not compatible
            -- throw away the results
            (pure None)
        _ -> pure $ Gain z
    )
    (pure $ Gain z)
mergeAmb x prev y = do
  -- In this case, there is no info to
  -- be gained from the previous results.
  -- Conflicts are ignored
  ifte
    (merge x (New (Filled y)))
    (\case
        Conflict -> pure prev
        z -> pure z)
    (pure prev)

-- Must be below the template haskell
instance Lattice TyCheckM (Hole (RtValF TyTerm)) where
  -- AMBIGUOUS VALUES
  merge x@(Old (Filled _)) (New (Ambiguous ys)) = do
    foldM (mergeAmb x) None ys
  merge (Old (Ambiguous xs)) y@(New (Filled _)) = do
    -- In this case, we filter down the
    -- ambiguous choices to only those
    -- that are compatible with the given
    -- info
    results <- traverse go xs
    case catMaybes $ toList results of
      -- All the possibilities were ruled
      [] -> pure Conflict
      -- Possibilities narrowed down to one
      (z:[]) -> pure . Gain $ Filled z
      (z:zs) -> if length zs == length xs - 1
        -- No possibilities were ruled out
        then pure None
        -- Some possibilities were ruled out
        else pure . Gain $ Ambiguous (z:|zs)
    where
      go x = ifte
        (merge (Old (Filled x)) y)
        (\case
            Gain (Filled x') -> pure $ Just x'
            -- Because we already know the old
            -- value was filled, we cannot loose
            -- information and become ambiguous or
            -- empty. If this ever happens, then
            -- the Lattice instance is in error
            Gain _ -> error "impossible"
            None -> pure $ Just x
            Conflict -> pure Nothing)
        (pure Nothing)

  merge (Old (Ambiguous xs)) (New (Ambiguous ys)) = do
    catMaybes . toList <$> (traverse go xs) >>= \case
      [] -> pure Conflict
      (z:[]) -> pure . Gain $ Filled z
      (z:zs) -> pure . Gain $ Ambiguous (z:|zs)

    where

      go x = do
        foldM (mergeAmb (Old $ Filled x)) None ys >>= \case
          Gain (Filled y) -> pure $ Just y
          Gain _ -> error "impossible"
          None -> pure $ Just x
          Conflict -> pure Nothing

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
  merge (Old (Filled (RtVarF i))) (New (Filled (RtVarF j))) = do
    pure $ if i == j
      then None
      else Conflict
  merge (Old (Filled (RtDepTyF i))) (New (Filled (RtDepTyF j))) = do
    pure $ if i == j
      then None
      else Conflict
  merge (Old (Filled x)) (New (Filled y)) = do
    xVal <- liftIO $ embed . fmap fst <$> traverse gatherTy x
    yVal <- liftIO $ embed . fmap fst <$> traverse gatherTy y
    error $ "Haven't fully implemented merge for TyTerms yet:\n" <> show xVal <> "\n" <> show yVal
  merge _ (New Empty) =  pure None
  merge (Old Empty) (New y) = pure $ Gain y

  bottom = pure Empty

  isTop (Filled _) = pure True
  isTop _ = pure False

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
treeGatherAllTys :: LocTree RowCol TyExprF -> IO (LocTree RowCol GatheredF, [TyTerm])
treeGatherAllTys (LocTree x y (TyExprF t val)) = do
  (t', ambs) <- gatherTy t
  tsF <- traverse treeGatherAllTys val
  let moreAmbs = foldr (<>) [] $ snd <$> tsF
  let ts = fst <$> tsF
  pure $ (LocTree x y $ GatheredF t' ts, ambs <> moreAmbs)

-- | Gather the annotated type for the root node into a value
treeGatherRootTy :: TyTree -> IO RtVal
treeGatherRootTy tree = fst <$> gatherTy (tyF $ locContent tree)

dropTypes :: LocTree l GatheredF -> RtVal
dropTypes tree = embed $ dropTypes <$> (bodyF $ locContent tree)

-- | Gather the term into a value such that all
-- constructors point directly to their children
-- instead of through a term reference
gatherTy :: TyTerm -> IO (RtVal, [TyTerm])
gatherTy c = do
  (result, (_,_,amb)) <- runStateT (go c) (Map.empty, 0, [])
  pure (result, amb)
  where
    go :: TyTerm -> StateT (Map Word32 Word32, Word32, [TyTerm]) IO RtVal
    go (TyTerm t) = do
      RootInfo thisCell _ uid _ _ <- liftIO $ rootInfo t
      liftIO (readRef $ value thisCell) >>= \case
        Filled (RtProdF xs) -> RtProd <$> traverse go xs
        Filled (RtArrF input output) ->  RtArr <$> go input <*> go output
        Filled RtTyF -> pure $ RtTy
        Filled (RtAppF func ins) -> RtApp <$> go func <*> traverse go ins
        Filled (RtPrimF _) -> undefined
        Filled (RtConF _ _) -> undefined
        Filled (RtVarF i) -> pure $ RtVar i
        Filled (RtDepTyF i) -> pure $ RtDepTy i
        Filled (RtLamF x) -> RtLam <$> go x
        Filled (RtUnknownF _) -> undefined
        Filled (RtAmbiguousF _) -> undefined
        Ambiguous xs -> do
          modify $ \(x,y,z) -> (x,y, TyTerm t:z)
          RtAmbiguous <$> traverse (fmap embed . traverse go) xs
        Empty -> do
          (tbl, n, as) <- get
          case Map.lookup uid tbl of
            Just i -> pure $ RtVar i
            Nothing -> do
              put $ (Map.insert uid n tbl, n + 1, as)
              pure $  RtUnknown n

tyExprToValue :: TyExpr -> IO RtVal
tyExprToValue t = embed <$> traverse tyExprToValue (expr t)

disperseTy :: RtVal -> TyCheckM TyTerm
disperseTy = cata (tyTerm . Filled <=< sequence)

-- | Strip the type term annotations from the tree
treeStripTypes :: TyTree -> RtVal
treeStripTypes (LocTree _ _ (TyExprF _ e)) =
  embed $ treeStripTypes <$> e

displayBindings :: Map Text Binding -> TyCheckM String
displayBindings bs = do
  pairs <- traverse go $ Map.assocs bs
  pure $ "{" <> (intercalate ", " $ pairs) <> "}"

  where

  go :: (Text, Binding) -> TyCheckM String
  go (_, LambdaBound x i) = do
    (t, _) <- liftIO $ gatherTy x
    pure $ "$" <> show i <> ": " <> displayRtVal t
  go (_, DepTyBound x i) = do
    (t, _) <- liftIO $ gatherTy x
    pure $ "#" <> show i <> ": " <> displayRtVal t
  go (name, Other x) = do
    val <- displayRtVal <$> liftIO (tyExprToValue x)
    pure $ Text.unpack name <> ": " <> val
