{-# LANGUAGE OverloadedStrings #-}

module Test.Parsing.Product where

import AST.Expr
import qualified AST.Expr as AST
import AST.Tree
import Control.Monad (void)
import Data.Text (Text, pack, unpack)
import Parsing.Grammar
import Parsing.Lexer (Token (..), lexer)
import Test.Parsing.Infra
import Test.Tasty
import Test.Tasty.HUnit

unit_product_empty :: IO ()
unit_product_empty = do
  result <- testParse "[]"
  spine result @?= Prod []

unit_product_two :: IO ()
unit_product_two = do
  result <- testParse "[[], []]"
  spine result @?= Prod [Prod [], Prod []]

unit_product_two_space_front :: IO ()
unit_product_two_space_front = do
  result <- testParse "[ [], []]"
  spine result @?= Prod [Prod [], Prod []]

unit_product_two_spaced :: IO ()
unit_product_two_spaced = do
  result <- testParse "[ [] , [] ]"
  spine result @?= Prod [Prod [], Prod []]

unit_product_lines :: IO ()
unit_product_lines = do
  result <-
    testParseLines
      [ "[ []",
        ", []",
        "]"
      ]
  spine result @?= Prod [Prod [], Prod []]
