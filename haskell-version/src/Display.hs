{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Display where

import Data.Functor.Const
import Data.Functor.Foldable (Fix, cata)
import Data.Sum
import Data.Text

class Display f where
  displayF :: f Text -> Text

instance Apply Display fs => Display (Sum fs) where
  displayF = apply @Display displayF

instance Display (Const Text) where
  displayF = getConst

display :: (Functor f, Display f) => Fix f -> Text
display = cata displayF
