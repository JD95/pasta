{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexer (Token (..), lex) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Either
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Data.Tuple
import Numeric.Natural
import Text.Read (readMaybe)
import Prelude hiding (lex)

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
  deriving (Eq, Show)

toMaybe :: Either e a -> Maybe a
toMaybe = either (const Nothing) Just

natNum :: Text -> Maybe Token
natNum = fmap (TNat . fst) . toMaybe . Text.decimal

intNum :: Text -> Maybe Token
intNum = fmap (TInt . fst) . toMaybe . Text.signed Text.decimal

dblNum :: Text -> Maybe Token
dblNum = fmap (TDbl . fst) . toMaybe . Text.signed Text.double

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

splits :: Text -> [(Text, Text)]
splits t = flip Text.splitAt t . (-) len <$> [0 .. len]
  where
    len = Text.length t

lexLargest :: Text -> Either () (Maybe (Token, Text))
lexLargest t
  | Text.null t = Left ()
  | otherwise =
    Right $ fmap swap . foldl' (<|>) Nothing $ fmap (\(chunk, rest) -> (,) rest <$> attempt chunk) (splits t)

lexSpaces :: Text -> [Token] -> (Text, [Token])
lexSpaces input tokens = (rest, tokens')
  where
    (spaces, rest) = Text.break ((/=) ' ') input
    tokens' = if len > 0 then TWhiteSpace len : tokens else tokens
      where
        len = fromIntegral $ Text.length spaces

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

lex :: Text -> Maybe [Token]
lex t = reverse <$> go t []
  where
    go :: Text -> [Token] -> Maybe [Token]
    go input tokens
      | Text.null input = Just tokens
      | otherwise = do
        let (input', tokens') = lexSpaces input tokens
        let (curr, rest) = Text.break ((==) ' ') input'
        results <- step curr
        go rest (reverse results <> tokens')
    step t = do
      case lexLargest t of
        Left _ -> Just []
        Right (Just (next, rest)) -> (next :) <$> step rest
        Right Nothing -> Nothing
