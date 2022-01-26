{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer (Pair (..), RowCol (..), Token (..), Lexeme (..), lexer) where

import Control.Applicative (Alternative, (<|>))
import Control.Monad (MonadPlus)
import Control.Monad.State (State, evalState)
import Data.Text (Text, pack)
import Numeric.Natural
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

data Pair = Open | Close
  deriving (Show, Eq)

data Lexeme
  = NewLine Natural
  | Space
  | Symbol Text
  | Paren Pair
  | Lambda
  | Arrow
  | Colon
  | Equals
  deriving (Show, Eq)

data RowCol = RowCol {row :: Pos, col :: Pos}
  deriving (Show, Eq, Ord)

data Token = Token {lexeme :: Lexeme, pos :: RowCol}
  deriving (Show, Eq)

data LexError = LexError
  deriving (Show, Eq, Ord)

data LexSt = LexSt

newtype Lex a = Lex {runLexer :: ParsecT LexError Text (State LexSt) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, MonadParsec LexError Text)

newLine :: Lex Lexeme
newLine = do
  _ <- char '\n'
  ss <- many (char ' ')
  pure $ NewLine . fromIntegral $ length ss

symbol :: Lex Lexeme
symbol = do
  hd <- letterChar
  tl <- many alphaNumChar
  pure . Symbol . pack $ hd : tl

token :: Lex Token
token = do
  SourcePos _ r c <- getSourcePos
  l <- parse
  pure $ Token l (RowCol r c)
  where
    parse =
      (Paren Open <$ char '(')
        <|> (Paren Close <$ char ')')
        <|> (Lambda <$ char '\\')
        <|> symbol
        <|> (Arrow <$ chunk "->")
        <|> (Colon <$ char ':')
        <|> (Equals <$ char '=')
        <|> (Space <$ some (char ' '))
        <|> newLine

lexer :: String -> Text -> Either (ParseErrorBundle Text LexError) [Token]
lexer srcFileName = flip evalState LexSt . runParserT (runLexer (many token)) srcFileName
