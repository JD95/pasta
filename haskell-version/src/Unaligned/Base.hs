{-# LANGUAGE PatternSynonyms #-}

-- | Catenable structures in the style of Purely Functional Data Structures
-- by Chris Okasaki
module Unaligned.Base
  ( View (..),
    Cons (..),
    Uncons (..),
    Snoc (..),
    Unsnoc (..),
    Nil (..),
    Singleton (..),
    Q,
    Cat,
    Rev (..),
    pattern Nil,
    pattern Cons,
    pattern Snoc,
  )
where

import Unaligned.Internal
