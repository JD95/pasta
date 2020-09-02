module Logic.Info where

-- |
--  if (isTop x) and isTop y,
--  then merge x y is a contradiction
--  unless they are equal.
class Merge a where
  merge :: a -> a -> Info a

  isTop :: a -> Bool

instance Merge Double where
  merge x y
    | x == y = NoInfo
    | otherwise = Contradiction

  isTop _ = True

data Info a
  = -- | Acts as multiplicative 0
    Contradiction
  | -- | Acts as multiplicative 1
    NoInfo
  | -- | Some info
    Info a
  deriving (Eq, Show)

instance Merge a => Semigroup (Info a) where
  (Info x) <> (Info y) = merge x y
  (Info x) <> NoInfo = Info x
  NoInfo <> (Info y) = Info y
  NoInfo <> NoInfo = NoInfo
  Contradiction <> _ = Contradiction
  _ <> Contradiction = Contradiction

instance Merge a => Monoid (Info a) where
  mempty = NoInfo

isInfo :: Info a -> Bool
isInfo (Info _) = True
isInfo _ = False

instance Functor Info where
  fmap f (Info x) = Info (f x)
  fmap _ NoInfo = NoInfo
  fmap _ Contradiction = Contradiction

instance Applicative Info where
  pure x = Info x

  (Info f) <*> (Info x) = Info (f x)
  NoInfo <*> _ = NoInfo
  _ <*> NoInfo = NoInfo
  Contradiction <*> _ = Contradiction
  _ <*> Contradiction = Contradiction
