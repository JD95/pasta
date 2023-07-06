module Perle.AST.Expr where

data Expr l h a
  = Lambda l a
  | Case Int [a]
  | App a a
  | Var a
  | Hole h

newtype SrcExpr = SrcExpr (Expr String (Maybe String) SrcExpr)
newtype RtExpr r m = RtExpr (Expr (Val r m) (Val r m) (Val r m))

{-
Another issue to work through: what exactly
will the tree workon and refine?

There shouldn't be some separate tree from the
one constructed by refinement.
-}

data RExpr r m
  | RLambda (Val r m) (Val r m)
  | RCase {- ??? -}

data RtVal
  = RtProd [RtVal]
  | RtVar Int

newtype Val r m = Val ()

data Env r m = Env [Val r m]

{-

Evaluation can happen via the propagators not the merges!
Reduction could happen via certain propagations that force eval
This allows the lattice code to be pure

-}

refine :: SrcExpr -> m (Val r m)
refine (Lambda _ body) =
  input <- newVal
  propLambda input (refine body)
refine (Case i x) = undefined
refine (Var i) = undefined

realize :: RtExpr r m -> m SrcExpr
realize _ = undefined

eval :: Val r m -> m (Val r m)
eval = undefined

newVal :: m (Valu r m)
newVal = undefined

stackPush :: Val r m -> m ()
stackPush x = undefined

stackPop :: m (Val r m)
stackPop x = undefined

withBound :: Val r m -> m a -> m a
withBound val action =
  stackPush val *> action <* stackPop

propEval :: Env r m -> Val r m -> Val r m -> m ()
propEval env input output = do
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
propUnify :: MonadRef m => Val r m -> Val r m -> m ()
propUnify x y = do
  push [Watched x] y
  push [Watched y] x


propLambda :: Val r m -> m (Val r m) -> m (Val r m)
propLambda input mkBody = do
  -- Construct the body of the function
  -- with the input on the stack, now info
  -- can propagate back
  body <- withBound input (void mkBody)
  newVal (RtExpr input body)

propApp :: MonadRef m => Val r m -> Val r m -> Val r m -> Env r m -> m ()
propApp func input output env = do
  body <- apply $ propEval env func
  term <- apply $ propEval env input
  push [Watched body, Watched term] output $ do
    x <- value term
    eval (x : env) =<< value body

propCase :: MonadRef m => Val r m -> [Val r m] -> Val r m -> Env r m -> m ()
propCase subject paths output env = do
  -- what would this be?
  result <- apply $ propEval env subject
  chosen <- newCell (Nothing :: Maybe Int)
  push [Watched result] chosen $ do
    value result >>= \case
      Case n _ -> pure (Just n)
      _ -> fail
  -- This will only ever fire once
  -- because chosen is a Maybe cell
  push [Watched chosen] output $ do
    x <- value result
    -- stackPush x
    -- somehow make x available to the
    -- chosen path
    let pick = paths !! n
    -- set this up so updates can
    -- still flow, the prop for result
    -- should really only happen once
    push [Watched pick] chosen
    value pick
  -- Wait, is propCase for eval or for
  -- refinement??? How do I know which is which?
  newVal (RtExpr (Case subject paths))
