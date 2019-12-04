{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Logic where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Data.Functor.Foldable
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE

-- A Guess represents possible values for some value being
-- solved for
newtype Guess s a = Guess { unGuess :: STRef s [a] } deriving (Eq)

-- A hole is a collection of Guess values toward some goal.
-- When two holes are unified, one is added as a guess for the other.
newtype Hole s a = Hole { unHole :: STRef s (NonEmpty (Guess s a)) } deriving (Eq)

getGuess :: Hole s a -> ST s (Guess s a)
getGuess (Hole h) = NE.head <$> readSTRef h

guessHole :: Hole s a -> Guess s a -> ST s ()
guessHole (Hole h) g = modifySTRef h (NE.cons g)

fillHole :: Hole s a -> a -> ST s ()
fillHole h val = do
  guess <- unGuess <$> getGuess h
  modifySTRef guess (val :)

unifyHole :: Hole s a -> Hole s a -> ST s ()
unifyHole holeA holeB = do
  when (holeA /= holeB) $ do
    guessHole holeA =<< getGuess holeB

newHole :: ST s (Hole s a)
newHole = do
  h     <- newSTRef []
  guess <- newSTRef (Guess h :| [])
  pure $ Hole guess

data Term s a = Var (Hole s a) | Val a

data Subst s a = Subst { substHole :: Hole s a, substStack :: [a] }

newtype Unfilled f s = Unfilled (f (Term s (Unfilled f s)))

class Traversable f => Unified f where
  merge :: Alternative t => (a -> b -> t c) -> f a -> f b -> t (f c)

unify :: (Traversable f, Unified f) => Unfilled f s -> Unfilled f s -> ST s ()
unify (Unfilled a) (Unfilled b) = do
  case merge (\x y -> Just (x, y)) a b of
    Just results -> forM_ results $ \case
      (Val a', Val b') -> unify a' b'
      (Val a', Var h ) -> fillHole h a'
      (Var h , Val b') -> fillHole h b'
      (Var a', Var b') -> unifyHole a' b'
    Nothing -> error "Unification Failed!"

extractFilled :: Traversable f => Unfilled f s -> ST s (Maybe (Fix f))
extractFilled (Unfilled x) = do
  a <- traverse extractTerm $ x
  case sequence a of
    Nothing -> pure Nothing
    Just b  -> do
      c <- traverse extractFilled $ b
      pure $ Fix <$> sequence c
 where
  extractTerm :: Term s a -> ST s (Maybe a)
  extractTerm (Var h) = extractHole h
  extractTerm (Val v) = pure $ Just v

  extractHole :: Hole s a -> ST s (Maybe a)
  extractHole h = do
    guess <- unGuess <$> getGuess h
    val   <- readSTRef guess
    pure $ case val of
      []      -> Nothing
      (v : _) -> Just v

data Tree a = Tree (Tree a) a (Tree a) | Leaf a deriving (Show, Functor, Foldable, Traversable)

instance Unified Tree where
  merge f (Tree s x t) (Tree s' x' t') = do
    lft <- merge f s s'
    v <- f x x'
    rht <- merge f t t'
    pure $ Tree lft v rht

  merge f (Leaf x) (Leaf y) = Leaf <$> f x y

test = do
  h1 <- newHole
  let x = Unfilled (Tree (Leaf $ Val (1 :: Int)) (Var h1) (Leaf $ Val 5))
  let y = Unfilled (Tree (Leaf $ Val (1 :: Int)) (Val 2) (Leaf $ Val 5))
  unify x y
  extractFilled x

