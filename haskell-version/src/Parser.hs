{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Parser (AST, parse, displayReport) where

import AST.Expr (ExprF (..))
import qualified AST.Expr as Expr
import AST.LocTree
import qualified AST.LocTree as Loc
import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import qualified Data.HashSet as HS
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Lexer (Lexeme, Pair (..), RowCol, Token (..), lexer)
import qualified Lexer as Lex
import Text.Earley hiding (parser)
import qualified Text.Earley as E
import Text.Earley.Mixfix

type AST = LocTree RowCol ExprF

parse :: [Token] -> ([AST], Report String [Token])
parse = fullParses (E.parser grammar)

displayReport :: [Token] -> Report String [Token] -> Text
displayReport input (Report i expected rest) =
  pack . unlines $
    [ "Parse Error at " <> show (pos (input !! (i - 1))),
      "expected: " <> show expected,
      "unconsumed: " <> (show $ lexeme <$> rest)
    ]

grammar :: Grammar r (Prod r String Token AST)
grammar = mdo
  let_ <-
    rule $
      triCon LetF
        <$> (symbol "let" *> some space *> someSymbol)
        <*> (spaced equals *> expr)
        <*> (spaced (symbol "in") *> expr)
  app <-
    rule $
      mkApp
        <$> expr
        <*> some (some space *> expr)
  ann <-
    rule $
      biCon AnnF
        <$> expr
        <*> (spaced colon *> expr)
  lam <-
    rule $
      biCon LamF
        <$> (lambda *> many space *> expr)
        <*> (spaced arr *> expr)
  expr <-
    rule $
      unit <|> between Lex.Paren expr <|> lam <|> let_ <|> ann <|> app <|> someSymbol
  pure expr

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
space = satisfy go
  where
    go (Token Lex.Space _) = True
    go _ = False

spaced :: Prod r String Token a -> Prod r String Token a
spaced x = some space *> x <* some space

lambda :: Prod r String Token Token
lambda = satisfy go <?> "lambda"
  where
    go (Token Lex.Lambda _) = True
    go _ = False

colon :: Prod r String Token Token
colon = satisfy go <?> "type annotation"
  where
    go (Token Lex.Colon _) = True
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

between :: (Pair -> Lexeme) -> Prod r String Token AST -> Prod r String Token AST
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

testParser :: IO ()
testParser = do
  let fileName = "examples/Example1.jy"
  txt <- Text.readFile fileName
  case lexer fileName txt of
    Right tokens -> do
      case parse tokens of
        ([], report) -> Text.putStrLn $ displayReport tokens report
        (result : _, _) -> Text.putStrLn $ Loc.fold (\_ _ -> Expr.display) result
    Left e -> putStrLn $ show e
