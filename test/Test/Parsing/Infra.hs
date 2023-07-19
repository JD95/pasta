{-# LANGUAGE OverloadedStrings #-}

module Test.Parsing.Infra (testParse, testLex, testParseLines) where

import AST.Expr
import qualified AST.Expr as AST
import AST.Expr.Source
import AST.LocTree
import Control.Monad (void)
import Data.Text (Text, unlines, unpack)
import Lexer (Token (..), lexer)
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (unlines)

testParseLines :: [Text] -> IO (AST Src)
testParseLines = testParse . unlines

testParse :: Text -> IO (AST Src)
testParse input = do
  tokens <- testLex input
  case parse tokens of
    ([], report) -> assertFailure (unpack $ "parse fail: " <> displayReport tokens report)
    (result : _, _) -> pure result

testLex :: Text -> IO [Token]
testLex input =
  case lexer "test" input of
    Right tokens -> pure tokens
    Left e -> assertFailure ("lex fail: " <> show e)
