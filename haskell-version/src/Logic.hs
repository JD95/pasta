{-# LANGUAGE LambdaCase #-}

module Logic where

import           Control.Arrow
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

data Term s a = Var (Hole s a) | Val a

data Subst s a = Subst { substHole :: Hole s a, substStack :: [a] }

newtype Unfilled f s a = Unfilled (f (Term s (Unfilled f s a)))

class ZipF f where
  zipF :: f a -> f a -> Maybe [(a,a)]

unify :: ZipF f => Unfilled f s a -> Unfilled f s a -> ST s ()
unify (Unfilled a) (Unfilled b) = do
  case zipF a b of
    Just results -> forM_ results $ \case
      (Val a', Val b') -> unify a' b'
      (Val a', Var h ) -> fillHole h a'
      (Var h , Val b') -> fillHole h b'
      (Var a', Var b') -> unifyHole a' b'
    Nothing -> error "Unification Failed!"
