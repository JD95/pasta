module Logic.Info where

class Merge a where
  merge :: a -> a -> Info a

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

instance Merge Double where
  merge x y
    | x == y = NoInfo
    | otherwise = Contradiction

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

newtype Justified b a = Justified {unJustified :: (b, Info a)}

instance (Merge a, Ord b, Monoid b) => Merge (Justified b a) where
  merge (Justified (j1, x)) (Justified (j2, y)) =
    case x <> y of
      NoInfo ->
        if j1 == j2
          then NoInfo
          else Info $ Justified (min j1 j2, x)
      Contradiction ->
        if j1 == j2
          then Info $ Justified (j1, Contradiction)
          else Info $ Justified (min j1 j2, Contradiction)
      Info z -> Info $ Justified (j1 <> j2, Info z)
