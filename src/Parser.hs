{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Parser (AST, parse, displayReport) where

import AST.Expr (ExprF (..))
import AST.LocTree
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Lexer (Lexeme, Pair (..), RowCol, Token (..))
import qualified Lexer as Lex
import Text.Earley hiding (parser)
import qualified Text.Earley as E
import Prelude hiding (product)

type AST = LocTree RowCol ExprF

parse :: [Token] -> ([AST], Report String [Token])
parse = fullParses (E.parser grammar)

displayReport :: [Token] -> Report String [Token] -> Text
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
  lvl2 <- rule $ depArr <|> nonDepArr <|> lvl3
  lvl3 <- rule $ app <|> lvl4
  lvl4 <- rule $ let_ <|> lvl5
  lvl5 <- rule $ lam <|> lvl6
  lvl6 <- rule $ someSymbol <|> unit <|> prod <|> between Lex.Paren expr
  let_ <-
    rule $
      triCon LetF
        <$> (symbol "let" *> some space *> someSymbol)
        <*> (spaced equals *> expr)
        <*> (spaced (symbol "in") *> expr)
  app <-
    rule $
      mkApp
        <$> lvl6
        <*> (some (some space *> lvl6) <|> indent (some (lvl6 <* many newline)))
  nonDepArr <- rule $ arrow lvl3 lvl2
  depArr <- rule $ depArrow lvl3 lvl2
  ann <- rule $ annotation lvl2
  lam <- rule $ lambda expr
  prod <- rule $ product expr
  pure (expr <* (many space <|> many newline))

indent :: Prod r String Token a -> Prod r String Token a
indent p = between Lex.Indent p <|> p

product :: Prod r String Token AST -> Prod r String Token AST
product expr =
  between Lex.Paren $
    many space
      *> (mk <$> some (expr <* many space <* comma) <*> (many space *> expr))
      <* many space
  where
    mk xs x = LocTree (locStart $ head xs) (locEnd x) $ ProdF (xs <> [x])

annotation :: Prod r String Token AST -> Prod r String Token AST
annotation expr =
  biCon AnnF
    <$> expr
    <*> (spaced colon *> expr <* many space)

depArrow ::
  Prod r String Token AST ->
  Prod r String Token AST ->
  Prod r String Token AST
depArrow _ yesRec = mk <$> input <*> (spaced arr *> yesRec <* many space)
  where
    mk ((start, n), inTy) outTy =
      LocTree start (locEnd outTy) (ArrF (Just n) inTy outTy)

    input =
      between Lex.Paren $
        (,)
          <$> (many space *> terminal name <* spaced colon)
          <*> (yesRec <* many space)

    name (Token (Lex.Symbol t) rc) = Just (rc, t)
    name _ = Nothing

arrow ::
  Prod r String Token AST ->
  Prod r String Token AST ->
  Prod r String Token AST
arrow noRec yesRec = mk <$> noRec <*> (spaced arr *> yesRec)
  where
    mk inTy outTy =
      LocTree (locStart inTy) (locEnd outTy) (ArrF Nothing inTy outTy)

lambda :: Prod r String Token AST -> Prod r String Token AST
lambda expr =
  (\start inputName body -> fromJust $ mkLocTree start (locEnd body) $ LamF inputName body)
    <$> (terminal lam <* many space)
    <*> terminal name
    <*> (spaced arr *> expr)
  where
    lam (Token Lex.Lambda rc) = Just rc
    lam _ = Nothing

    name (Token (Lex.Symbol s) _) = pure s
    name _ = Nothing

symbol :: Text -> Prod r String Token AST
symbol t = terminal go <?> "symbol"
  where
    go (Token (Lex.Symbol s) rc) = guard (t == s) *> (mkLocTree rc rc $ SymbolF t)
    go _ = Nothing

someSymbol :: Prod r String Token AST
someSymbol = terminal go <?> "symbol"
  where
    go (Token (Lex.Symbol t) rc) = mkLocTree rc rc $ SymbolF t
    go _ = Nothing

arr :: Prod r String Token Token
arr = satisfy go <?> "arrow"
  where
    go (Token Lex.Arrow _) = True
    go _ = False

space :: Prod r String Token Token
space = satisfy s
  where
    s (Token Lex.Space _) = True
    s _ = False

newline :: Prod r String Token Token
newline = satisfy s
  where
    s (Token Lex.NewLine _) = True
    s _ = False

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

biCon :: (AST -> AST -> ExprF AST) -> AST -> AST -> AST
biCon f input body = fromJust $ mkLocTree (locStart input) (locEnd body) (f input body)

triCon :: (AST -> AST -> AST -> ExprF AST) -> AST -> AST -> AST -> AST
triCon f x y z = fromJust $ mkLocTree (locStart x) (locEnd z) (f x y z)

mkApp :: AST -> [AST] -> AST
mkApp f xs = fromJust $ mkLocTree (locStart f) (locEnd $ last xs) (AppF f xs)
