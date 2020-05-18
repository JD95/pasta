{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Display where

import Data.Sum
import Data.Functor.Foldable (Fix, cata)
import Data.Text

class Display f where
  displayF :: f Text -> Text

instance Apply Display fs => Display (Sum fs) where
  displayF = apply @Display displayF

display :: (Functor f, Display f) => Fix f -> Text
display = cata displayF 
