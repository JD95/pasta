{-# LANGUAGE OverloadedStrings #-}

module Test.Parsing.Application where

import AST.Expr
import qualified AST.Expr as AST
import AST.LocTree
import Control.Monad (void)
import Data.Text (Text, pack, unpack)
import Lexer (Token (..), lexer)
import Parser
import Test.Parsing.Infra
import Test.Tasty
import Test.Tasty.HUnit

unit_singleLine :: IO ()
unit_singleLine = do
  result <-
    testParse $ "someFunction input1 input2"
  spine result @?= App (Symbol "someFunction") [Symbol "input1", Symbol "input2"]

unit_block :: IO ()
unit_block = do
  result <-
    testParseLines
      [ "someFunction",
        "  input1",
        "  input2"
      ]
  spine result @?= App (Symbol "someFunction") [Symbol "input1", Symbol "input2"]

unit_funcInParens :: IO ()
unit_funcInParens = do
  result <- testParse "(f x) y"
  spine result @?= App (App (Symbol "f") [Symbol "x"]) [Symbol "y"]

unit_funcInParensBlock :: IO ()
unit_funcInParensBlock = do
  result <-
    testParseLines
      [ "(f x)",
        "  y"
      ]
  spine result @?= App (App (Symbol "f") [Symbol "x"]) [Symbol "y"]

unit_inputInParens :: IO ()
unit_inputInParens = do
  result <- testParse "f (x y)"
  spine result @?= App (Symbol "f") [App (Symbol "x") [Symbol "y"]]

unit_inputInParensBlock :: IO ()
unit_inputInParensBlock = do
  result <-
    testParseLines
      [ "f",
        "  (x y)"
      ]
  spine result @?= App (Symbol "f") [App (Symbol "x") [Symbol "y"]]
