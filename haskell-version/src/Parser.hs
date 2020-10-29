{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parser (parse) where

import AST.Core hiding (app)
import AST.Surface
import AST.Transform
import Control.Monad.Free
import Data.Functor.Foldable
import Data.Sum
import Parser.Lexer
import RIO
import RIO.List
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Error (ParseErrorBundle)

data ParseErr = LErr LexError deriving (Eq, Ord, Show)

type Parser = Parsec ParseErr [Lexeme]

instance Ord a => MP.Stream [a] where
  type Token [a] = a
  type Tokens [a] = [a]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ [] = Nothing
  take1_ (t : ts) = Just (t, ts)
  takeN_ n s
    | n <= 0 = Just ([], s)
    | null s = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span
  showTokens = undefined
  reachOffset = undefined

expr :: Parser (Fix Surface)
expr = MP.try (paren expr) <|> MP.try lambda <|> freeVar

symbol :: Parser (Text, Row, Col)
symbol = MP.token go mempty
  where
    go = \case
      (Lexeme (TSymbol t) r c) -> Just (t, r, c)
      _ -> Nothing

freeVar :: Parser (Fix Surface)
freeVar = do
  (t, r, c) <- symbol
  pure . Fix $ Node (inject $ FreeVar t) r c

tok :: Token -> Parser (Row, Col)
tok given = MP.token go mempty
  where
    go (Lexeme x r c) =
      if x == given then Just (r, c) else Nothing

lambda :: Parser (Fix Surface)
lambda = do
  (r, c) <- tok TLambda
  _ <- optional space
  (x, _, _) <- symbol
  _ <- optional space
  _ <- tok TArrow
  _ <- optional space
  Fix body <- expr
  _ <- optional space
  pure . Fix $ Parent (inject $ Lam x (Fix body)) (r, c) (end body)

space :: Parser ()
space = do
  flip MP.token mempty $ \case
    (Lexeme (TWhiteSpace _) _ _) -> Just ()
    _ -> Nothing

app :: Parser (Fix Surface)
app = do
  Fix func <- MP.try (paren app) <|> expr
  asApp func <|> pure (Fix func)
  where
    asApp x = do
      _ <- space
      Fix input <- expr
      pure . Fix $ Parent (inject $ App (Fix x) (Fix input)) (start x) (end input)

paren :: Parser (Fix Surface) -> Parser (Fix Surface)
paren f = do
  _ <- tok TLParen
  _ <- optional space
  x <- f
  _ <- optional space
  _ <- tok TRParen
  pure x

lang :: Parsec ParseErr [Lexeme] (Fix Surface)
lang = MP.try (paren app) <|> MP.try app <|> expr

parse :: Text -> Either (ParseErrorBundle [Lexeme] ParseErr) (Fix Surface)
parse input =
  case lex input of
    Right xs -> MP.parse lang "" xs
    Left e -> Left undefined
