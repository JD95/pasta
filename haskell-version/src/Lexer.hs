{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Control.Applicative (Alternative, (<|>))
import Control.Monad (MonadPlus, void)
import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State
import Data.Text
import Numeric.Natural
import Text.Megaparsec (MonadParsec, ParseErrorBundle, ParsecT, between, chunk, many, runParserT, takeWhileP, try)
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

data Bracket = Open | Close

data Token
  = Space Natural
  | Symbol Text
  | Paren Bracket

data LexError = LexError
  deriving (Show, Eq, Ord)

data LexSt = LexSt

newtype Lex a = Lex {runLexer :: ParsecT LexError Text (State LexSt) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, MonadParsec LexError Text)

space :: Lex ()
space = L.space space1 lineComment blockComment
  where
    lineComment = void $ chunk "--"

    blockComment :: Lex ()
    blockComment = do
      let start = chunk "{-"
      let end = chunk "-}"
      let body :: Lex ()
          body = do
            _ <- takeWhileP Nothing (/= '-')
            try (void $ char '}') <|> body
      between start end body

lexer :: String -> Text -> Either (ParseErrorBundle Text LexError) [Token]
lexer srcFileName = flip evalState LexSt . runParserT (runLexer go) srcFileName
  where
    go :: Lex [Token]
    go = many (char '(' *> pure (Paren Open))
