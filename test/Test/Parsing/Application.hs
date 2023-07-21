{-# LANGUAGE OverloadedStrings #-}

module Test.Parsing.Application where

import AST.Expr
import AST.Tree
import Test.Parsing.Infra
import Test.Tasty.HUnit

unit_application_single_line :: IO ()
unit_application_single_line = do
  result <-
    testParse $ "someFunction input1 input2"
  spine result @?= App (Symbol "someFunction") [Symbol "input1", Symbol "input2"]

unit_application_block :: IO ()
unit_application_block = do
  result <-
    testParseLines
      [ "someFunction",
        "  input1",
        "  input2"
      ]
  spine result @?= App (Symbol "someFunction") [Symbol "input1", Symbol "input2"]

unit_application_func_in_parens :: IO ()
unit_application_func_in_parens = do
  result <- testParse "(f x) y"
  spine result @?= App (App (Symbol "f") [Symbol "x"]) [Symbol "y"]

unit_application_func_in_parens_block :: IO ()
unit_application_func_in_parens_block = do
  result <-
    testParseLines
      [ "(f x)",
        "  y"
      ]
  spine result @?= App (App (Symbol "f") [Symbol "x"]) [Symbol "y"]

unit_application_input_in_parens :: IO ()
unit_application_input_in_parens = do
  result <- testParse "f (x y)"
  spine result @?= App (Symbol "f") [App (Symbol "x") [Symbol "y"]]

unit_application_input_in_parens_block :: IO ()
unit_application_input_in_parens_block = do
  result <-
    testParseLines
      [ "f",
        "  (x y)"
      ]
  spine result @?= App (Symbol "f") [App (Symbol "x") [Symbol "y"]]
