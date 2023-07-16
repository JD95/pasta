{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer
  ( Pair (..),
    RowCol (..),
    Token (..),
    Lexeme (..),
    Content (..),
    Grouping (..),
    lexer,
    displayLexeme,
  )
where

import Control.Applicative (Alternative, (<|>))
import Control.Monad (MonadPlus)
import Control.Monad.State.Strict (MonadState (..), State, evalState, get, modify)
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
  deriving (Show, Eq, Ord)

data Grouping
  = Space
  | NewLine
  | Indent Pair
  | Paren Pair
  deriving (Show, Eq, Ord)

data Content
  = Symbol Text
  | Lambda
  | Arrow
  | Colon
  | Comma
  | Equals
  deriving (Show, Eq, Ord)

newtype Lexeme = Lexeme {unLexeme :: Either Grouping Content}
  deriving (Show, Eq, Ord)

displayContent :: Content -> Text
displayContent (Symbol t) = t
displayContent (Lambda) = "\\"
displayContent (Arrow) = "->"
displayContent (Colon) = ":"
displayContent (Comma) = ","
displayContent (Equals) = "="

displayGrouping :: Grouping -> Text
displayGrouping (Paren Open) = "("
displayGrouping (Paren Close) = ")"
displayGrouping NewLine = "\\n"
displayGrouping (Indent Open) = "\\n{"
displayGrouping (Indent Close) = "\\n}"
displayGrouping Space = " "

displayLexeme :: Lexeme -> Text
displayLexeme (Lexeme (Left w)) = displayGrouping w
displayLexeme (Lexeme (Right w)) = displayContent w

data RowCol = RowCol {row :: Pos, col :: Pos}
  deriving (Show, Eq, Ord)

data Token = Token {lexeme :: Lexeme, pos :: RowCol}
  deriving (Show, Eq)

data LexError = LexError
  deriving (Show, Eq, Ord)

data LexSt = LexSt {indent :: [Int]}

newtype Lex a = Lex {runLexer :: ParsecT LexError Text (State LexSt) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, MonadParsec LexError Text, MonadState LexSt)

grouping :: Grouping -> Lexeme
grouping = Lexeme . Left

content :: Content -> Lexeme
content = Lexeme . Right

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
          pure [grouping $ Indent Open]
        else pure [grouping NewLine]
    (n : _) ->
      if
          | n == m ->
            -- Same identation level
            pure [grouping NewLine]
          | n < m -> do
            -- Deeper indentation
            modify $ \st -> st {indent = m : ns}
            pure $ [grouping $ Indent Open]
          | otherwise -> do
            -- Shallower indentation
            let (toClose, remaining) = span (m <) ns
            modify $ \st -> st {indent = remaining}
            pure $ grouping (Indent Close) <$ toClose

symbol :: Lex Lexeme
symbol = do
  hd <- letterChar
  tl <- many alphaNumChar
  pure . content . Symbol . pack $ hd : tl

token :: Lex [Token]
token = do
  SourcePos _ r c <- getSourcePos
  l <- ((: []) <$> parse) <|> newLine
  pure $ Token <$> l <*> pure (RowCol r c)
  where
    parse =
      (grouping (Paren Open) <$ char '(')
        <|> (grouping (Paren Close) <$ char ')')
        <|> (content Lambda <$ char '\\')
        <|> symbol
        <|> (content Arrow <$ chunk "->")
        <|> (content Colon <$ char ':')
        <|> (content Comma <$ char ',')
        <|> (content Equals <$ char '=')
        <|> (grouping Space <$ some (char ' '))

lexTokens :: Lex [Token]
lexTokens = do
  ts <- many token
  SourcePos _ r c <- getSourcePos
  ns <- indent <$> get
  let endIndents = Token (grouping $ Indent Close) (RowCol r c) <$ ns
  pure $ concat ts <> endIndents

lexer :: String -> Text -> Either (ParseErrorBundle Text LexError) [Token]
lexer srcFileName = flip evalState initSt . runParserT (runLexer lexTokens) srcFileName
  where
    initSt = LexSt []
