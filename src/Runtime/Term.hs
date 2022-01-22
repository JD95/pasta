{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Runtime.Term where

import Control.Applicative
import Control.Monad
import Data.Foldable (for_)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Runtime.Ref
import Runtime.Prop

data Content uid r a
  = Root
      -- | Term value
      a
      -- | Rank
      Int
      -- | Terms this one is *not* equal to
      (HashSet (Term uid r a))
  | Child (Term uid r a)

data Term uid r a = Term
  { termId :: uid,
    termRef :: r (Content uid r a)
  }

instance Eq uid => Eq (Term uid r a) where
  x == y = termId x == termId y

instance Hashable uid => Hashable (Term uid r a) where
  hash = hash . termId
  hashWithSalt d = hashWithSalt d . termId

newTerm :: (Hashable uid, Ref m r) => uid -> a -> m (Term uid r a)
newTerm uid value = Term uid <$> newRef (Root value 0 HS.empty)

-- | Find the root info for a term, optimizing references when possible
--
-- If the term is already a root term, do nothing
-- If the term is a child, rewrite it so it's a direct child of the root
rootInfo :: (Hashable uid, Ref m r) => Term uid r a -> m (a, Int, HashSet (Term uid r a), Term uid r a)
rootInfo d =
  readRef (termRef d) >>= \case
    Root a i xs -> pure (a, i, xs, d)
    Child s -> do
      x@(_, _, _, parent) <- rootInfo s
      writeRef (termRef d) (Child parent)
      pure x

-- | Find the root info for a term, optimizing references when possible
--
-- If the term is already a root term, do nothing
-- If the term is a child, rewrite it so it's a direct child of the root
root :: (Hashable uid, Ref m r) => Term uid r a -> m (a, Term uid r a)
root d =
  readRef (termRef d) >>= \case
    Root value _ _ -> pure (value, d)
    Child s -> do
      result@(_, x) <- root s
      writeRef (termRef d) (Child x)
      pure result

overrideDisjunction :: (Eq uid, Hashable uid, Ref m r, Alternative m) => Term uid r a -> Term uid r a -> Term uid r a -> m ()
overrideDisjunction old new term = do
  backTrackingModifyRef (termRef term) $ \case
    (Root ivalue irank noti) -> Root ivalue irank $ HS.insert new $ HS.delete old noti
    child -> child

is :: (Eq uid, Hashable uid, Ref m r, Alternative m, Lattice m a)
  => Term uid r (Cell m r a)
  -> Term uid r (Cell m r a)
  -> m ()
is m n = do
  (mvalue, mrank, notm, mroot) <- rootInfo m
  (nvalue, nrank, notn, nroot) <- rootInfo n
  guard $ not $ HS.member mroot notn
  case compare mrank nrank of
    LT -> do
      inform nvalue =<< readRef (value mvalue)
      writeRef (termRef mroot) $ Child nroot
      for_ notm $ overrideDisjunction mroot nroot
      writeRef (termRef nroot) $ Root nvalue nrank $ notm <> notn
    GT -> do
      inform mvalue =<< readRef (value nvalue)
      writeRef (termRef nroot) $ Child mroot
      for_ notn $ overrideDisjunction nroot mroot
      writeRef (termRef mroot) $ Root mvalue mrank $ notm <> notm
    EQ -> do
      inform nvalue =<< readRef (value mvalue)
      writeRef (termRef mroot) $ Child nroot
      for_ notm $ overrideDisjunction mroot nroot
      writeRef (termRef nroot) $ Root nvalue (nrank + 1) $ notm <> notn

{-
isn't :: MonadRef m => TermM m -> TermM m -> m ()
isn't m n = do
  (mrank, notm, mroot) <- findEx m
  (nrank, notn, nroot) <- findEx n
  guard $ mroot /= nroot
  writeRef mroot $ Root mrank $ HS.insert nroot notm
  writeRef nroot $ Root nrank $ HS.insert mroot notn

-- | ground out an equality relation
decide :: MonadRef m => TermM m -> TermM m -> m Bool
decide m n =
  True <$ is m n
    <|> False <$ isn't m n
-}
