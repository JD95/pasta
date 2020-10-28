{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Display where

import Data.Functor.Const
import Data.Functor.Foldable (Fix, cata)
import Data.Sum
import RIO

class DisplayF f where
  displayF :: f Text -> Text

instance Apply DisplayF fs => DisplayF (Sum fs) where
  displayF = apply @DisplayF displayF

instance DisplayF (Const Text) where
  displayF = getConst

display :: (Functor f, DisplayF f) => Fix f -> Text
display = cata displayF
