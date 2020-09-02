{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Logic.Monad
  ( LogicT (LogicT), -- TODO internal
    Logic,
    observe,
    observeMany,
    observeAll,
    observeT,
    observeManyT,
    observeAllT,
    -- TODO internal
    view,
    unview,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail as Fail
import Control.Monad.Primitive
import Control.Monad.Trans
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Identity
import Unaligned.Base

class MonadPlus m => MonadLogic m where
  -- |
  -- @
  -- msplit empty ≡ pure Empty
  -- msplit (pure a <|> m) == pure (a :&: m)
  -- @
  msplit :: m a -> m (View a (m a))

  -- | fair disjunction
  interleave :: m a -> m a -> m a
  interleave m1 m2 =
    msplit m1 >>= \case
      Empty -> m2
      a :&: m1' -> return a `mplus` interleave m2 m1'

  -- | fair conjunction
  (>>-) :: m a -> (a -> m b) -> m b
  m >>- f = do
    (a, m') <-
      msplit m >>= \case
        Empty -> mzero
        a :&: m' -> return (a, m')
    interleave (f a) (m' >>- f)

  -- |
  -- @
  -- ifte (return a) th el           == th a
  -- ifte mzero th el                == el
  -- ifte (return a `mplus` m) th el == th a `mplus` (m >>= th)
  -- @
  ifte :: m a -> (a -> m b) -> m b -> m b
  ifte t th el =
    msplit t >>= \case
      Empty -> el
      a :&: m -> th a <|> (m >>= th)

  -- | pruning
  once :: m a -> m a
  once m =
    msplit m >>= \case
      Empty -> empty
      a :&: _ -> pure a

-- |
-- @
-- msplit >=> reflect ≡ id
-- @
reflect :: Alternative m => View a (m a) -> m a
reflect Empty = empty
reflect (a :&: m) = pure a <|> m

lnot :: MonadLogic m => m a -> m ()
lnot m = ifte (once m) (const mzero) (return ())

type L m a = View a (LogicT m a)

type Logic = LogicT Identity

instance Nil (LogicT m) where
  nil = LogicT mempty

instance Monad m => Cons (LogicT m) where
  cons a xs = pure a <|> xs

instance Monad m => Snoc (LogicT m) where
  snoc xs a = xs <|> pure a

instance m ~ Identity => Uncons (LogicT m) where
  uncons = runIdentity . view

instance Monad m => Semigroup (LogicT m a) where
  (<>) = (<|>)

instance Monad m => Monoid (LogicT m a) where
  mempty = LogicT mempty

newtype LogicT m a = LogicT {runLogicT :: Cat (m (L m a))}

instance Functor m => Functor (LogicT m) where
  fmap f = LogicT . fmap (fmap (bimap f (fmap f))) . runLogicT

instance Foldable m => Foldable (LogicT m) where
  foldMap f = foldMap (foldMap (bifoldMap f (foldMap f))) . runLogicT

instance Traversable m => Traversable (LogicT m) where
  traverse f = fmap LogicT . traverse (traverse (bitraverse f (traverse f))) . runLogicT

single :: Monad m => a -> m (L m a)
single a = pure (a :&: empty)

unview :: m (L m a) -> LogicT m a
unview = LogicT . singleton

view :: Monad m => LogicT m a -> m (L m a)
view (LogicT s) = case uncons s of
  Empty -> return Empty
  h :&: t ->
    h >>= \case
      Empty -> view (LogicT t)
      hi :&: LogicT ti -> return $ hi :&: LogicT (ti <> t)

instance Monad m => Applicative (LogicT m) where
  pure = unview . single
  (<*>) = ap

instance Monad m => Alternative (LogicT m) where
  empty = LogicT mempty
  LogicT m <|> LogicT n = LogicT (m <> n)

instance Monad m => Monad (LogicT m) where
  m >>= f =
    unview $
      view m >>= \case
        Empty -> return Empty
        h :&: t -> view $ f h <|> (t >>= f)

#if __GLASGOW_HASKELL__ < 808
  fail _ = mzero
#endif

instance Monad m => Fail.MonadFail (LogicT m) where
  fail _ = mzero

instance Monad m => MonadPlus (LogicT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans LogicT where
  lift m = unview $ m >>= single

instance MonadIO m => MonadIO (LogicT m) where
  liftIO = lift . liftIO

instance Monad m => MonadLogic (LogicT m) where
  msplit = lift . view

instance PrimMonad m => PrimMonad (LogicT m) where
  type PrimState (LogicT m) = PrimState m
  primitive f = lift $ primitive f

observe :: Logic a -> a
observe m = runIdentity $ view m >>= go
  where
    go (a :&: _) = return a
    go _ = return (error "no results")

observeMany :: Int -> Logic a -> [a]
observeMany n = runIdentity . observeManyT n

observeAll :: Logic a -> [a]
observeAll m = go (runIdentity (view m))
  where
    go :: forall a. View a (Logic a) -> [a]
    go (a :&: t) = a : observeAll t
    go _ = []

observeT :: MonadFail m => LogicT m a -> m a
observeT m = view m >>= go
  where
    go (a :&: _) = return a
    go _ = Fail.fail "No results"

observeManyT :: Monad m => Int -> LogicT m a -> m [a]
observeManyT n m
  | n <= 0 = return []
  | otherwise =
    view m >>= \case
      Empty -> return []
      a :&: m1 -> (a :) <$> observeManyT (n -1) m1

observeAllT :: Monad m => LogicT m a -> m [a]
observeAllT m = view m >>= go
  where
    go (a :&: t) = (a :) <$> observeAllT t
    go _ = return []
