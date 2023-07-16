{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Parser (AST, parse, displayReport) where

import AST.Expr (ExprF (..), Src)
import AST.LocTree
import Control.Applicative
import Control.Monad
import Control.Monad.Writer.Strict
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Grouper
import Lexer (Content (..), Grouping (..), Lexeme, Pair (..), RowCol, Token (..))
import qualified Lexer as Lex
import Text.Earley hiding (parser)
import qualified Text.Earley as E
import Prelude hiding (product)

type AST = LocTree RowCol (ExprF Src)

parse :: [Token] -> ([AST], Report String [Token])
parse = undefined -- fullParses (E.parser grammar)

displayReport :: [Token] -> Report String [Token] -> Text
displayReport = undefined

{-
displayReport input (Report i ex rest) =
  pack . unlines $
    [ "Parse Error at " <> displayErrPos (pos (input !! (i - 1))),
      "expected: " <> show ex,
      "unconsumed: " <> concat (unpack . Lex.displayLexeme . lexeme <$> rest)
    ]
  where
    displayErrPos (Lex.RowCol r c) = show r <> ":" <> show c

grammar :: Grammar r (Prod r String Token AST)
grammar = mdo
  expr <- rule $ ann <|> lvl2
  lvl2 <- rule $ arr <|> lvl3
  lvl3 <- rule $ app <|> lvl4
  lvl4 <- rule $ let_ <|> lvl5
  lvl5 <- rule $ lam <|> lvl6
  lvl6 <- rule $ someSymbol <|> unit <|> prod <|> between Lex.Paren expr
  let_ <-
    rule . fmap constr $
      LetF
        <$> (symbol "let" *> some space *> someSymbol)
        <*> (spaced equals *> expr)
        <*> (spaced (symbol "in") *> expr)
  app <- rule $ application lvl6
  arr <- rule $ arrow lvl3 lvl2
  ann <- rule $ annotation lvl2
  lam <- rule $ lambda expr
  prod <- rule $ product expr
  pure (expr <* (many space <|> many newline))

snoc :: [a] -> a -> [a]
snoc xs x = xs <> [x]

application :: Prod r String Token AST -> Prod r String Token AST
application expr =
  fmap constr $
    AppF
      <$> expr
      <*> ( between
              Lex.Indent
              (snoc <$> many (expr <* some (space <|> newline)) <*> expr)
              <|> some (some space *> expr)
          )

product :: Prod r String Token AST -> Prod r String Token AST
product expr =
  between Lex.Paren $
    many space
      *> (mk <$> some (expr <* many space <* comma) <*> (many space *> expr))
      <* many space
  where
    mk xs x = LocTree (locStart $ head xs) (locEnd x) . ProdF $ snoc xs x

annotation :: Prod r String Token AST -> Prod r String Token AST
annotation expr =
  fmap constr $ AnnF <$> expr <*> (indAnn <|> nonIndAnn)
  where
    indAnn = many space *> between Lex.Indent (colon *> some space *> expr)
    nonIndAnn = space *> colon *> (indExpr <|> (space *> nonIndExpr))
    indExpr = between Lex.Indent nonIndExpr
    nonIndExpr = expr <* many space

arrow ::
  Prod r String Token AST ->
  Prod r String Token AST ->
  Prod r String Token AST
arrow noRec yesRec = mk <$> input <*> ((space <|> newline) *> arr *> output)
  where
    mk (Nothing, inTy) outTy =
      LocTree (locStart inTy) (locEnd outTy) (ArrF Nothing inTy outTy)
    mk (Just (start, n), inTy) outTy =
      LocTree start (locEnd outTy) (ArrF (Just n) inTy outTy)

    input =
      ( between Lex.Paren $
          (,)
            <$> (Just <$> (many space *> terminal name <* spaced colon))
            <*> (yesRec <* many space)
      )
        <|> ((,) <$> pure Nothing <*> noRec)

    output =
      (between Lex.Indent yesRec)
        <|> (newline *> yesRec)
        <|> (space *> yesRec)

    name (Token (Lex.Symbol t) rc) = Just (rc, t)
    name _ = Nothing

lambda :: Prod r String Token AST -> Prod r String Token AST
lambda expr =
  prepend
    <$> (terminal lam <* many space)
    <*> fmap constr (LamF <$> terminal name <*> (spaced arr *> expr))
  where
    lam (Token Lex.Lambda rc) = Just rc
    lam _ = Nothing

    name (Token (Lex.Symbol s) _) = pure s
    name _ = Nothing

symbol :: Text -> Prod r String Token AST
symbol t = terminal go <?> "symbol"
  where
    go (Token (Lex.Symbol s) rc) =
      guard (t == s) *> (mkLocTree rc rc $ SymbolF t)
    go _ = Nothing

someSymbol :: Prod r String Token AST
someSymbol = terminal go <?> "symbol"
  where
    go (Token (Lex.Symbol t) rc) =
      mkLocTree rc rc $ SymbolF t
    go _ = Nothing

arr :: Prod r String Token Token
arr = satisfy go <?> "arrow"
  where
    go (Token Lex.Arrow _) = True
    go _ = False

space :: Prod r String Token Token
space = satisfy $ \case
  (Token Lex.Space _) -> True
  _ -> False

newline :: Prod r String Token Token
newline = satisfy $ \case
  (Token Lex.NewLine _) -> True
  _ -> False

spaced :: Prod r String Token a -> Prod r String Token a
spaced x = some space *> x <* some space

colon :: Prod r String Token Token
colon = satisfy go <?> "type annotation"
  where
    go (Token Lex.Colon _) = True
    go _ = False

comma :: Prod r String Token Token
comma = satisfy go <?> "comma"
  where
    go (Token Lex.Comma _) = True
    go _ = False

equals :: Prod r String Token Token
equals = satisfy go <?> "="
  where
    go (Token Lex.Equals _) = True
    go _ = False

unit :: Prod r String Token AST
unit =
  (\l r -> fromJust $ mkLocTree (pos l) (pos r) (ProdF []))
    <$> (go open <?> show open)
    <*> (go close <?> show close)
  where
    open = Lex.Paren Open
    close = Lex.Paren Close
    go x = satisfy (\(Token y _) -> x == y)

between :: (Pair -> Lexeme) -> Prod r String Token a -> Prod r String Token a
between p e = (go open <?> show open) *> e <* (go close <?> show close)
  where
    open = p Open
    close = p Close
    go x = satisfy (\(Token y _) -> x == y)

data LocRange
  = LocRange RowCol RowCol
  | LocRangeEmpty

instance Semigroup LocRange where
  LocRangeEmpty <> LocRangeEmpty = LocRangeEmpty
  x@(LocRange _ _) <> LocRangeEmpty = x
  LocRangeEmpty <> y@(LocRange _ _) = y
  (LocRange xStart xEnd) <> (LocRange yStart yEnd) =
    LocRange (min xStart yStart) (max xEnd yEnd)

instance Monoid LocRange where
  mempty = LocRangeEmpty

constr :: ExprF Src AST -> AST
constr this =
  case runWriter $ traverse go this of
    (body, LocRange start end) -> fromJust $ mkLocTree start end body
    (_, LocRangeEmpty) -> undefined
  where
    go x = writer (x, LocRange (locStart x) (locEnd x))
-}
