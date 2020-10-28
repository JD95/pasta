{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parser.Lexer (Lexeme (..), Row (..), Col (..), Token (..), lex) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error (Error)
import qualified Control.Monad.Freer.Error as Err
import Control.Monad.Freer.State (State)
import qualified Control.Monad.Freer.State as ST
import Data.Char
import Data.Either
import Data.List
import qualified Data.Text.Read as Read
import Data.Tuple
import Numeric.Natural
import RIO
import qualified RIO.Text as Text
import Text.Read (readMaybe)

data Token
  = TNat Natural
  | TInt Int
  | TDbl Double
  | TQuote
  | TDblQuote
  | TLambda
  | TArrow
  | TLParen
  | TRParen
  | TColon
  | TSymbol Text
  | TWhiteSpace Natural
  | TNewLine
  deriving (Eq, Show)

newtype Row = Row {unRow :: Natural} deriving (Num, Enum, Eq, Ord, Show)

newtype Col = Col {unCol :: Natural} deriving (Num, Enum, Eq, Ord, Show)

data Lexeme = Lexeme !Token !Row !Col deriving (Eq, Show)

toMaybe :: Either e a -> Maybe a
toMaybe = either (const Nothing) Just

natNum :: Text -> Maybe Token
natNum = fmap (TNat . fst) . toMaybe . Read.decimal

intNum :: Text -> Maybe Token
intNum = fmap (TInt . fst) . toMaybe . Read.signed Read.decimal

dblNum :: Text -> Maybe Token
dblNum = fmap (TDbl . fst) . toMaybe . Read.signed Read.double

quote :: Text -> Maybe Token
quote t = guard ("'" == t) *> Just TQuote

dblQuote :: Text -> Maybe Token
dblQuote t = guard ("\"" == t) *> Just TDblQuote

lambda :: Text -> Maybe Token
lambda t = guard ("\\" == t) *> Just TLambda

arrow :: Text -> Maybe Token
arrow t = guard ("->" == t) *> Just TArrow

lParen :: Text -> Maybe Token
lParen t = guard ("(" == t) *> Just TLParen

rParen :: Text -> Maybe Token
rParen t = guard (")" == t) *> Just TRParen

colon :: Text -> Maybe Token
colon t = guard (":" == t) *> Just TColon

symbol :: Text -> Maybe Token
symbol t = do
  (h, rest) <- Text.uncons t
  guard $ isAlpha h
  guard . all isAlphaNum $ Text.unpack rest
  pure $ TSymbol t

newLine :: Text -> Maybe Token
newLine t = guard ("\n" == t) *> Just TNewLine

splits :: Text -> [(Text, Text)]
splits t = flip Text.splitAt t . (-) len <$> [0 .. len]
  where
    len = Text.length t

lexLargest :: Text -> Either () (Maybe (Token, Text))
lexLargest t
  | Text.null t = Left ()
  | otherwise =
    Right $ fmap swap . foldl' (<|>) Nothing $ fmap (\(chunk, rest) -> (,) rest <$> attempt chunk) (splits t)

lexSpaces :: Members '[State LexST] es => Eff es ()
lexSpaces = do
  st <- ST.get
  let (spaces, rest) = Text.break ((/=) ' ') $ input st
  let len = fromIntegral $ Text.length spaces
  when (len > 0) $ do
    ST.modify $ \st ->
      st
        { tokens = Lexeme (TWhiteSpace len) (row st) (col st) : tokens st,
          col = col st + Col len
        }
  ST.modify (\st -> st {input = rest})

attempt t =
  natNum t
    <|> intNum t
    <|> quote t
    <|> dblQuote t
    <|> lambda t
    <|> arrow t
    <|> lParen t
    <|> rParen t
    <|> colon t
    <|> symbol t
    <|> newLine t

data LexST = LexST {row :: !Row, col :: !Col, input :: !Text, tokens :: ![Lexeme]}

data LexError = CannotLex !Text !Row !Col deriving (Eq, Show)

breakInput :: Members '[State LexST] es => (Char -> Bool) -> Eff es Text
breakInput pred = do
  (chunk, input') <- Text.break pred . input <$> ST.get
  ST.modify $ \st -> st {input = input'}
  pure chunk

lex :: Text -> Either LexError [Lexeme]
lex t = run . Err.runError . fmap (reverse . tokens) . ST.execState (LexST (Row 0) (Col 0) t []) $ go
  where
    go :: Members '[State LexST, Error LexError] es => Eff es ()
    go = do
      st <- ST.get
      unless (Text.null $ input st) $ do
        lexSpaces
        chunk <- breakInput ((==) ' ')
        step chunk
        go

    step :: Members '[State LexST, Error LexError] es => Text -> Eff es ()
    step t = do
      st <- ST.get
      case lexLargest t of
        Left _ -> pure ()
        Right (Just (next, rest)) -> do
          ST.modify $ \st ->
            st
              { tokens = Lexeme next (row st) (col st) : tokens st,
                col = case next of
                  TNewLine -> 0
                  _ -> (col st) + Col (fromIntegral $ Text.length t - Text.length rest),
                row = case next of
                  TNewLine -> row st + 1
                  _ -> row st
              }
          step rest
        Right Nothing -> Err.throwError $ CannotLex t (row st) (col st)
