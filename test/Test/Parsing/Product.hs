{-# LANGUAGE OverloadedStrings #-}

module Test.Parsing.Product where

import AST.Expr
import AST.Tree
import Test.Parsing.Infra
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
