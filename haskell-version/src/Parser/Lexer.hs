{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parser.Lexer (LexError (..), Lexeme (..), Row (..), Col (..), Token (..), lex) where

import AST.Transform
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
  | TLambda
  | TArrow
  | TLParen
  | TRParen
  | TColon
  | TSymbol Text
  | TString Text
  | TWhiteSpace Natural
  | TNewLine
  deriving (Eq, Ord, Show)

instance RIO.Display Token where
  textDisplay (TNat n) = textDisplay $ (fromIntegral n :: Int)
  textDisplay (TLParen) = "("
  textDisplay (TRParen) = ")"
  textDisplay (TSymbol t) = t

data Lexeme = Lexeme !Token !Row !Col deriving (Eq, Ord, Show)

instance RIO.Display Lexeme where
  textDisplay (Lexeme t _ _) = textDisplay t

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

lexString :: Members '[State LexST] es => Eff es ()
lexString = do
  st <- ST.get
  let (string, rest) = Text.break ((==) '"') $ input st
  let len = fromIntegral $ Text.length string
  let endQuote = 1
  ST.modify $ \st ->
    st
      { tokens = Lexeme (TString string) (row st) (col st) : tokens st,
        col = col st + Col len + endQuote
      }
  ST.modify (\st -> st {input = Text.drop 1 rest})

attempt t =
  natNum t
    <|> intNum t
    <|> quote t
    <|> lambda t
    <|> arrow t
    <|> lParen t
    <|> rParen t
    <|> colon t
    <|> symbol t
    <|> newLine t

data LexST = LexST {row :: !Row, col :: !Col, input :: !Text, tokens :: ![Lexeme]}

data LexError = CannotLex !Text !Row !Col deriving (Eq, Ord, Show)

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
        st' <- ST.get
        case Text.uncons (input st') of
          Just ('"', _) -> do
            ST.modify $ \x -> x {input = Text.drop 1 (input x)}
            lexString
            go
          Just _ -> do
            chunk <- breakInput ((==) ' ')
            step chunk
            go

    step :: Members '[State LexST, Error LexError] es => Text -> Eff es ()
    step t = do
      st <- ST.get
      case Text.uncons t of
        Just ('"', _) -> ST.modify $ \x -> x {input = t <> (input x)}
        _ -> case lexLargest t of
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
