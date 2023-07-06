module Runtime.Lattice where

import Data.Functor.Identity

data Info a
  = Gain a
  | None
  | Conflict

instance Functor Info where
  fmap f (Gain x) = Gain $ f x
  fmap _ None = None
  fmap _ Conflict = Conflict

instance Applicative Info where
  pure x = Gain x

  (Gain f) <*> (Gain x) = Gain $ f x
  (Gain _) <*> None = None
  (Gain _) <*> Conflict = Conflict
  None <*> (Gain _) = None
  None <*> None = None
  None <*> Conflict = None
  Conflict <*> (Gain _) = Conflict
  Conflict <*> None = Conflict
  Conflict <*> Conflict = Conflict

data Lattice a = Lattice
  { isTop :: a -> Bool,
    bottom :: a,
    merge :: a -> a -> Info a
  }

data LatticeM m a = LatticeM
  { isTopM :: a -> m Bool,
    bottomM :: m a,
    mergeM :: a -> a -> m (Info a)
  }

impure :: Lattice a -> LatticeM Identity a
impure f =
  LatticeM
    { isTopM = Identity . isTop f,
      bottomM = Identity $ bottom f,
      mergeM = \x y -> Identity $ merge f x y
    }

maybe :: Lattice (Maybe a)
maybe =
  Lattice
    { isTop = isJust,
      bottom = Nothing,
      merge = _
    }

productM :: Monad m => LatticeM m a -> LatticeM m b -> LatticeM m (a, b)
productM f g =
  LatticeM
    { isTopM = \(a, b) ->
        (&&) <$> isTopM f a <*> isTopM g b,
      bottomM =
        (,) <$> bottomM f <*> bottomM g,
      mergeM = \(a, b) (x, y) -> do
        first <- mergeM f a x
        second <- mergeM g b y
        pure $ (,) <$> first <*> second
    }
