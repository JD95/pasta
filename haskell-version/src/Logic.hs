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

newtype Unfilled f s a = Unfilled (f (Term s a))

class Traversable f => Unified f where
  merge :: Alternative t => (a -> b -> t c) -> f a -> f b -> t (f c)

unify :: (Traversable f, Unified f, Eq a) => Unfilled f s a -> Unfilled f s a -> ST s ()
unify (Unfilled a) (Unfilled b) = do
  case merge (\x y -> Just (x, y)) a b of
    Just results -> forM_ results $ \case
      (Val  _, Val _ ) -> pure ()
      (Val a', Var h ) -> fillHole h a'
      (Var h , Val b') -> fillHole h b'
      (Var a', Var b') -> unifyHole a' b'
    Nothing -> error "Unification Failed!"

resolveHole :: Hole s a -> ST s (Maybe a)
resolveHole h = do
  guess <- unGuess <$> getGuess h
  val   <- readSTRef guess
  pure $ case val of
    []      -> Nothing
    (v : _) -> Just v

resolveTerm :: Term s a -> ST s (Maybe a)
resolveTerm (Var h) = resolveHole h
resolveTerm (Val v) = pure $ Just v

resolveLayer :: Traversable f => Unfilled f s a -> ST s (Maybe (f a))
resolveLayer (Unfilled x) = fmap sequence $ traverse resolveTerm x

resolve :: Traversable f => Fix (Unfilled f s) -> ST s (Maybe (Fix f))
resolve (Fix f) = do
  layer <- resolveLayer f
  case layer of
    Nothing -> pure Nothing
    Just t -> do
      t' <- traverse resolve t
      pure . fmap Fix . sequence $ t'

instance Unified []  where
  merge f (x:xs) (y:ys) = (:) <$> f x y <*> merge f xs ys
  merge _ [] [] = pure [] 
  merge _ _ _ = empty 

test = runST $ do
  h1 <- newHole
  let x = Unfilled [Val "apple", Val "cake"]
  let y = Unfilled [Val "apple", Var h1] 
  unify x y
  resolveLayer y

