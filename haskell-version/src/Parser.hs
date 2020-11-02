{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import RIO.Text (unpack)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Error (ParseErrorBundle, ShowErrorComponent (..))
import TypeCheck.Typed.Stages hiding (ann)

data ParseErr = LErr LexError deriving (Eq, Ord, Show)

instance ShowErrorComponent ParseErr where
  showErrorComponent _ = "fail"

type Parser = Parsec ParseErr [Lexeme]

instance (RIO.Display a, Ord a) => MP.Stream [a] where
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
  showTokens _ xs = unpack $ foldr (<>) "" $ textDisplay <$> toList xs
  reachOffset = undefined

symbol :: Parser (Text, Row, Col)
symbol = MP.token go mempty
  where
    go = \case
      (Lexeme (TSymbol t) r c) -> Just (t, r, c)
      _ -> Nothing

freeVar :: Parser (Fix Surface)
freeVar = do
  (t, r, c) <- symbol
  pure $ surface (inject $ FreeVar t) (Point r c)

tok :: Token -> Parser (Row, Col)
tok given = MP.token go mempty
  where
    go (Lexeme x r c) =
      if x == given then Just (r, c) else Nothing

lambda :: Parser (Fix Surface) -> Parser (Fix Surface)
lambda inner = do
  (r, c) <- tok TLambda
  _ <- optional space
  (x, _, _) <- symbol
  _ <- optional space
  _ <- tok TArrow
  _ <- optional space
  Fix body <- inner
  _ <- optional space
  pure $ surface (inject $ Lam x (Fix body)) (Range (r, c) (end body))

ann :: Parser (Fix Surface) -> Parser (Fix Surface) -> Parser (Fix Surface)
ann l r = do
  term <- MP.try (paren r) <|> l
  asAnn term <|> pure term
  where
    asAnn x = do
      _ <- optional space
      _ <- tok TColon
      _ <- optional space
      ty <- r
      pure $ surface (inject $ Ann x ty) (Range (start $ unfix x) (end $ unfix ty))

space :: Parser ()
space = do
  flip MP.token mempty $ \case
    (Lexeme (TWhiteSpace _) _ _) -> Just ()
    _ -> Nothing

app :: Parser (Fix Surface) -> Parser (Fix Surface) -> Parser (Fix Surface)
app l r = do
  func <- MP.try (paren r) <|> l
  _ <- space
  inputs <- r `MP.sepBy1` space
  pure $ foldl' applyInput func inputs
  where
    applyInput x y = surface (inject $ App x y) (Range (start $ unfix x) (end $ unfix y))

paren :: Parser (Fix Surface) -> Parser (Fix Surface)
paren f = do
  _ <- tok TLParen
  _ <- optional space
  x <- f
  _ <- optional space
  _ <- tok TRParen
  pure x

lang :: Parsec ParseErr [Lexeme] (Fix Surface)
lang = MP.try (paren lvl1) <|> lvl1
  where
    lvl1 = MP.try (lvl0 `app` lvl1) <|> MP.try (lvl0 `ann` lvl1) <|> lvl0
    lvl0 = MP.try (paren lvl0) <|> MP.try (lambda lvl1) <|> freeVar

parse :: Text -> Either (ParseErrorBundle [Lexeme] ParseErr) (Fix Surface)
parse input =
  case lex input of
    Right xs -> MP.parse lang "" xs
    Left e -> Left undefined
