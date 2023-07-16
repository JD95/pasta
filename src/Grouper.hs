{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grouper where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.Text
import Lexer
import Text.Megaparsec
  ( MonadParsec,
    ParseErrorBundle,
    ParsecT,
    SourcePos (..),
    chunk,
    getSourcePos,
    many,
    runParserT,
    some,
  )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar)
import Text.Megaparsec.Pos (Pos)

data Block a
  = Spaced [a]
  | Cont [a] a [Block a]
  deriving (Functor)

data GroupingError = GroupingError
  deriving (Eq, Ord)

newtype Group a = Group {runGrouper :: ParsecT GroupingError [Lexeme] Identity a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, MonadParsec GroupingError [Lexeme])

grouper :: String -> Text -> Either (ParseErrorBundle Text GroupingError) (Block Content)
grouper = undefined
