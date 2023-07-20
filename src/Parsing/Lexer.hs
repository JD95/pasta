{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing.Lexer
  ( Pair (..),
    RowCol (..),
    Token (..),
    Lexeme (..),
    LexError (..),
    lexer,
    displayLexeme,
  )
where

import Control.Applicative (Alternative, (<|>))
import Control.Monad (MonadPlus)
import Control.Monad.State.Strict (MonadState (..), State, evalState, get, modify)
import Data.Bifunctor
import Data.Text (Text, pack)
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
  = Indent Pair
  | NewLine
  | Space
  | Symbol Text
  | Paren Pair
  | Lambda
  | Arrow
  | Colon
  | Comma
  | Equals
  deriving (Show, Eq)

displayLexeme :: Lexeme -> Text
displayLexeme NewLine = "\\n"
displayLexeme (Indent Open) = "\\i{"
displayLexeme (Indent Close) = "\\i}"
displayLexeme Space = " "
displayLexeme (Symbol t) = t
displayLexeme (Paren Open) = "("
displayLexeme (Paren Close) = ")"
displayLexeme (Lambda) = "\\"
displayLexeme (Arrow) = "->"
displayLexeme (Colon) = ":"
displayLexeme (Comma) = ","
displayLexeme (Equals) = "="

data RowCol = RowCol {row :: Pos, col :: Pos}
  deriving (Show, Eq, Ord)

data Token = Token {lexeme :: Lexeme, pos :: RowCol}
  deriving (Show, Eq)

data LexError = LexError
  deriving (Show, Eq, Ord)

data LexSt = LexSt {indent :: [Int]}

newtype Lex a = Lex {runLexer :: ParsecT LexError Text (State LexSt) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, MonadParsec LexError Text, MonadState LexSt)

newLine :: Lex [Lexeme]
newLine = do
  _ <- char '\n'
  ss <- many (char ' ')
  ns <- indent <$> get
  let m = length ss
  case ns of
    [] ->
      if m > 0
        then do
          modify $ \st -> st {indent = [m]}
          pure [Indent Open]
        else pure [NewLine]
    (n : _) ->
      if
          | n == m -> pure [NewLine]
          | n < m -> do
            modify $ \st -> st {indent = m : ns}
            pure $ [Indent Open]
          | otherwise -> do
            let (toClose, remaining) = span (m <) ns
            modify $ \st -> st {indent = remaining}
            pure $ Indent Close <$ toClose

symbol :: Lex Lexeme
symbol = do
  hd <- letterChar
  tl <- many alphaNumChar
  pure . Symbol . pack $ hd : tl

token :: Lex [Token]
token = do
  SourcePos _ r c <- getSourcePos
  l <- ((: []) <$> parse) <|> newLine
  pure $ Token <$> l <*> pure (RowCol r c)
  where
    parse =
      (Paren Open <$ char '(')
        <|> (Paren Close <$ char ')')
        <|> (Lambda <$ char '\\')
        <|> symbol
        <|> (Arrow <$ chunk "->")
        <|> (Colon <$ char ':')
        <|> (Comma <$ char ',')
        <|> (Equals <$ char '=')
        <|> (Space <$ some (char ' '))

lexTokens :: Lex [Token]
lexTokens = do
  ts <- many token
  SourcePos _ r c <- getSourcePos
  ns <- indent <$> get
  let endIndents = Token (Indent Close) (RowCol r c) <$ ns
  pure $ concat ts <> endIndents

lexer :: String -> Text -> Either LexError [Token]
lexer srcFileName = first (const LexError) . flip evalState initSt . runParserT (runLexer lexTokens) srcFileName
  where
    initSt = LexSt []
