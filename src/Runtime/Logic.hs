{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Runtime.Logic where

import Control.Applicative
import Control.Monad.Logic
import Control.Monad.State
import Data.Foldable
import Data.Functor.Identity
import Data.Kind
import Data.List (nub)
import Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Traversable
import Debug.Trace
import Prelude hiding (Bool (..))

data Part = Atom | Rel | App | Clause | Def

data Logical (p :: Part) where
  -- Terms
  -- Share with Functional?
  Sym :: Text -> Logical 'Atom
  True :: Logical 'Atom
  False :: Logical 'Atom
  -- Applications
  -- Share with Functional?
  Var :: Text -> Logical 'App
  AppAtom :: Logical 'Atom -> Logical 'App
  Apply :: Text -> [Logical 'App] -> Logical 'App
  -- Relations
  RelApp :: Logical 'App -> Logical 'Rel
  Unify :: Logical 'App -> Logical 'App -> Logical 'Rel
  -- Clauses
  ClauseRel :: Logical 'Rel -> Logical 'Clause
  And :: [Logical 'Clause] -> Logical 'Clause
  Or :: [Logical 'Clause] -> Logical 'Clause
  -- Definitions
  Pred :: Text -> [Logical 'App] -> Logical 'Clause -> Logical 'Def

deriving instance Eq (Logical p)

deriving instance Show (Logical p)

class LiftTerm t (f :: t -> Type) (p :: t) (q :: t) where
  liftTerm :: f p -> f q

instance LiftTerm t f a a where
  liftTerm = id

instance LiftTerm Part Logical 'Atom 'App where
  liftTerm = AppAtom

instance LiftTerm Part Logical 'Atom 'Rel where
  liftTerm = RelApp . AppAtom

instance LiftTerm Part Logical 'App 'Rel where
  liftTerm = RelApp

instance LiftTerm Part Logical 'Rel 'Clause where
  liftTerm = ClauseRel

instance LiftTerm Part Logical 'App 'Clause where
  liftTerm = ClauseRel . RelApp

sym :: LiftTerm Part Logical 'Atom p => Text -> Logical p
sym t = liftTerm $ Sym t

var :: LiftTerm Part Logical 'App p => Text -> Logical p
var t = liftTerm $ Var t

app :: LiftTerm Part Logical 'App p => Text -> [Logical 'App] -> Logical p
app t ts = liftTerm $ Apply t ts

is :: Logical 'App -> Logical 'App -> Logical 'Clause
is x y = ClauseRel $ Unify x y

data RtVal
  = Value (Logical 'Atom)
  | Definition (Logical 'Def)
  deriving (Eq, Show)

data BackTrackSt
  = BackTrackSt (Map Text RtVal) (Maybe BackTrackSt)

data EvalSt = EvalSt
  { defs :: Map Text (Logical 'Def),
    vars :: Map (Int, Text) RtVal,
    substitutions :: Map (Int, Text) (Int, Text),
    goal :: Int
  }

type EvalM a = LogicT (State EvalSt) a

eval :: Logical 'Clause -> EvalSt -> Logic [(Text, Logical 'App)]
eval query initSt = getResults $ do
  goClause query
  case clauseVars query of
    [] -> pure [("Query", AppAtom True)]
    xs -> for xs $ \x -> do
      st <- get
      case Map.lookup (0, x) (vars st) of
        Just (Value v) -> pure $ (x, AppAtom v)
        Just _ -> error "var is predicate"
        Nothing -> do
          (_, y) <- lookupVarSubst (0, x)
          if x == y
            then error $ "var " <> show x <> " not instantiated"
            else pure (x, Var y)
  where
    getResults :: EvalM a -> Logic a
    getResults = hoistLogicT $ (Identity . flip evalState initSt)

    goClause :: Logical 'Clause -> EvalM ()
    goClause (And cs) = do
      -- traceM "eval and"
      traverse_ goClause cs
    goClause (Or (x : xs)) = do
      -- traceM "eval or"
      st <- get
      goClause x <|> (put st *> goClause (Or xs))
    goClause (Or []) = Control.Applicative.empty
    goClause (ClauseRel cs) = goRel cs

    goRel :: Logical 'Rel -> EvalM ()
    goRel (RelApp a) = goApp a
    goRel (Unify x y) = do
      -- traceM "eval unify"
      g <- goal <$> get
      unify (g, x) (g, y)

    goApp :: Logical 'App -> EvalM ()
    goApp (Var _) = pure ()
    goApp (AppAtom _) = pure ()
    goApp (Apply name xs) = do
      lookupPred name >>= \case
        Just p -> goDef p xs
        Nothing -> error "invalid functor"

    goDef :: Logical 'Def -> [Logical 'App] -> EvalM ()
    goDef (Pred name ys clauses) xs = do
      -- traceM $ "calling " <> show name
      thisGoal <- goal <$> get
      let nextGoal = thisGoal + 1
      modify $ \st -> st {goal = nextGoal}
      goClause clauses
      traverse_ (uncurry unify) (Prelude.zip ((,) thisGoal <$> xs) ((,) nextGoal <$> ys))

fromLogicTWith ::
  (Applicative m, Monad n, Alternative n) =>
  (forall x. m x -> n x) ->
  LogicT m a ->
  n a
fromLogicTWith p (LogicT f) =
  join . p $
    f (\a v -> pure (pure a <|> join (p v))) (pure Control.Applicative.empty)

hoistLogicT :: (Applicative m, Monad n) => (forall x. m x -> n x) -> LogicT m a -> LogicT n a
hoistLogicT f = fromLogicTWith (lift . f)

unify :: (Int, Logical 'App) -> (Int, Logical 'App) -> EvalM ()
unify (g, Var x) (h, Var y) = do
  x' <- lookupVarSubst (g, x)
  y' <- lookupVarSubst (h, y)
  unless (x' == y') $ do
    vs <- vars <$> get
    case (Map.lookup x' vs, Map.lookup y' vs) of
      -- Unify with a left bias
      -- No matter what, y will be
      -- replaced with x
      (Just xVal, Just yVal) -> do
        guard $ xVal == yVal
        -- traceM "success"
        subst x' y'
      (Just _, Nothing) ->
        subst x' y'
      (Nothing, Just yVal) -> do
        -- even if y is the only one with a value
        -- give that value to x and replace it
        modify $ \st -> st {vars = Map.insert x' yVal (vars st)}
        subst x' y'
      (Nothing, Nothing) ->
        subst x' y'
unify (g, Var x) (_, AppAtom a) = do
  -- traceM $ show (g, x) <> " = " <> show a
  unifyWithAtom (g, x) a
unify (_, AppAtom a) (h, Var y) = do
  -- traceM $ show a <> " = " <> show (h, y)
  unifyWithAtom (h, y) a
unify (_, AppAtom a) (_, AppAtom b) = guard $ a == b
unify (_, Var _) (_, Apply _ _) = error "Unifying applications not supported"
unify (_, Apply _ _) (_, Var _) = error "Unifying applications not supported"
unify (_, Apply _ _) (_, Apply _ _) = error "Unifying applications not supported"
unify (_, AppAtom _) (_, Apply _ _) = do
  -- traceM "unifying atom vs apply"
  Control.Applicative.empty
unify (_, Apply _ _) (_, AppAtom _) = do
  -- traceM "unifying apply vs atom"
  Control.Applicative.empty

unifyWithAtom :: (Int, Text) -> Logical 'Atom -> EvalM ()
unifyWithAtom x a = do
  y <- lookupVarSubst x
  lookupVarValue y >>= \case
    Just (Value xVal) -> do
      guard $ xVal == a
    -- traceM "success"
    Just (Definition _) -> do
      traceM "unifying against definition"
      Control.Applicative.empty
    Nothing -> do
      modify $ \st -> st {vars = Map.insert y (Value a) (vars st)}

-- traceM "success"

clauseVars :: Logical 'Clause -> [Text]
clauseVars = nub . go
  where
    go :: Logical 'Clause -> [Text]
    go = \case
      (And xs) -> clauseVars =<< xs
      (Or xs) -> clauseVars =<< xs
      (ClauseRel xs) -> innerRel xs

    innerRel :: Logical 'Rel -> [Text]
    innerRel (RelApp x) = innerApp x
    innerRel (Unify x y) = innerApp x <> innerApp y

    innerApp :: Logical 'App -> [Text]
    innerApp (Var x) = [x]
    innerApp (AppAtom _) = []
    innerApp (Apply _ as) = innerApp =<< as

lookupPred :: Text -> EvalM (Maybe (Logical 'Def))
lookupPred name = Map.lookup name . defs <$> get

lookupVarSubst :: (Int, Text) -> EvalM (Int, Text)
lookupVarSubst t = do
  ss <- substitutions <$> get
  let f name = case Map.lookup name ss of
        Just sub -> f sub
        Nothing -> name
  pure $ f t

lookupVarValue :: (Int, Text) -> EvalM (Maybe RtVal)
lookupVarValue x = do
  x' <- lookupVarSubst x
  vs <- vars <$> get
  pure $ Map.lookup x' vs

subst :: (Int, Text) -> (Int, Text) -> EvalM ()
subst x y = do
  y' <- lookupVarSubst y
  unless (x == y') $ do
    modify $ \st -> st {substitutions = Map.insert y' x (substitutions st)}

edge :: Logical 'Def
edge =
  Pred
    "edge"
    [var "X", var "Y"]
    ( Or
        [ And [var "X" `is` sym "a", var "Y" `is` sym "b"],
          And [var "X" `is` sym "a", var "Y" `is` sym "c"],
          And [var "X" `is` sym "c", var "Y" `is` sym "d"]
        ]
    )

path :: Logical 'Def
path =
  Pred
    "path"
    [var "X", var "Y"]
    ( Or
        [ app "edge" [var "X", var "Y"],
          And
            [ app "edge" [var "X", var "Z"],
              app "path" [var "Z", var "Y"]
            ]
        ]
    )

testSt :: EvalSt
testSt =
  EvalSt
    { defs = Map.fromList [("path", path), ("edge", edge)],
      vars = Map.empty,
      substitutions = Map.empty,
      goal = 0
    }

displayResult :: (Text, Logical 'App) -> Text
displayResult (name, x) = name <> " = " <> displayApp x
  where
    displayApp :: Logical 'App -> Text
    displayApp = \case
      AppAtom (Sym val) -> Text.pack $ show val
      AppAtom True -> "true"
      AppAtom False -> "false"
      Var val -> val
      Apply p xs -> p <> "(" <> Text.intercalate ", " (displayApp <$> xs) <> ")"

testQuery :: Logical 'Clause -> [[Text]]
testQuery x = fmap displayResult <$> observeAll (eval x testSt)

test :: IO ()
test = do
  for_ (observeAll $ eval (app "path" [var "X", var "Y"]) testSt) $ \case
    [] -> pure ()
    results -> do
      traverse_ (Text.putStrLn . displayResult) results
      putStr " "
      input <- getLine
      if input == "."
        then error "exiting!"
        else pure ()
