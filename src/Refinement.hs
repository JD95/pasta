{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Refinement where

import AST.Expr
import Control.Applicative
import Control.Monad.Reader
import Lattice
import Runtime.Prop
import Runtime.Ref

type Rt m = 'ExprConfig (Val m) (Val m) (Val m)

{-
Another issue to work through: what exactly
will the tree workon and refine?

There shouldn't be some separate tree from the
one constructed by refinement.
-}

data RExpr m
  = RLambda (Val m) (Val m)
  | RCase {- ??? -}

data RtVal
  = RtProd [RtVal]
  | RtVar Int
  | RtLam RtVal
  | RtHole Int
  | Unbound

instance Lattice RtVal where
  bottom = Unbound
  merge (Old old) (New new) =
    case (old, new) of
      (Unbound, Unbound) -> None
  isTop _ = False

data Val m where
  Val :: (Value f, Inform f, MonadRef m) => f m (Ref m) RtVal -> Val m

data Env m = Env [Val m]

class
  ( MonadReader (Env m) m,
    Alternative m,
    MonadRef m
  ) =>
  MonadRefine m
  where
  newHoleId :: m Int

{-

Evaluation can happen via the propagators not the merges!
Reduction could happen via certain propagations that force eval
This allows the lattice code to be pure

-}

refine :: MonadRefine m => Expr Src -> m (Expr (Rt m))
refine (Hole _) = do
  holeId <- newHoleId
  Hole <$> newVal (RtHole holeId)

--   (Lambda _ body) -> do
--     input <- newVal Unbound
--     stackPush input
--     propLambda input =<< refine body
--   (Case i x) -> undefined
--   (Var i) -> undefined

realize :: Expr (Rt m) -> m (Expr Src)
realize _ = undefined

eval :: Env m -> RtVal -> m RtVal
eval = undefined

newVal :: RtVal -> m (Val m)
newVal = undefined

stackPush :: Val m -> m ()
stackPush x = undefined

stackPop :: m (Val m)
stackPop = undefined

withBound :: Applicative m => Val m -> m a -> m a
withBound val action =
  stackPush val *> action <* stackPop

propEval ::
  (Alternative m, MonadRef m) =>
  Env m ->
  Val m ->
  Cell m (Ref m) RtVal ->
  m ()
propEval env (Val input) output = do
  push [Watched input] output $ do
    eval env =<< value input

{-

The current problem here is: how to backprop. info from
the reduced value to it's sources?

For example,

x = (\a -> a) ?

It should be able to deduce that `?` is `x`

I think this means that each form that can reduce needs
to have it's own way of push info back. I think there are
only two cases, function application and case expressions

The big challenge is how to represent the stack

-}
-- propUnify :: Cell m (Ref m) RtVal -> Cell m (Ref m) RtVal -> m ()
-- propUnify x y = do
--   push [Watched x] y (value x)
--   push [Watched y] x (value y)
--
-- propLambda :: Val m -> m (Val m) -> m (RtExpr m)
-- propLambda input mkBody = do
--   -- Construct the body of the function
--   -- with the input on the stack, now info
--   -- can propagate back
--   body <- withBound input (mkBody)
--   newVal (RtExpr (RtLam input body))
--
-- propApp :: MonadRef m => Val m -> Val m -> Val m -> Env m -> m ()
-- propApp func input output env = do
--   body <- apply $ propEval env func
--   term <- apply $ propEval env input
--   push [Watched body, Watched term] output $ do
--     x <- value term
--     eval (x : env) =<< value body
--
-- propCase :: MonadRef m => Val m -> [Val m] -> Val m -> Env m -> m ()
-- propCase subject paths output env = do
--   -- what would this be?
--   result <- apply $ propEval env subject
--   chosen <- newCell (Nothing :: Maybe Int)
--   push [Watched result] chosen $ do
--     value result >>= \case
--       Case n _ -> pure (Just n)
--       _ -> fail
--   -- This will only ever fire once
--   -- because chosen is a Maybe cell
--   push [Watched chosen] output $ do
--     x <- value result
--     -- stackPush x
--     -- somehow make x available to the
--     -- chosen path
--     let pick = paths !! n
--     -- set this up so updates can
--     -- still flow, the prop for result
--     -- should really only happen once
--     push [Watched pick] chosen
--     value pick
--   -- Wait, is propCase for eval or for
--   -- refinement??? How do I know which is which?
--   newVal (RtExpr (Case subject paths))
