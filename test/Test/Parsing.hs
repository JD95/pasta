{-# LANGUAGE OverloadedStrings #-}

module Test.Parsing where

import AST.Expr
import qualified AST.Expr as AST
import AST.LocTree
import Control.Monad (void)
import Data.Text (Text, unpack)
import Lexer (Token (..), lexer)
import Parser
import Test.Tasty
import Test.Tasty.HUnit

tests =
  testGroup
    "parsing"
    [ testCase "() parses" unitParses,
      testCase "example parses" exampleParses,
      testCase "Annotations parse" annParses,
      testCase "arrows parse" parseArrNonDep,
      testCase "dependent arrows parse" parseArrDep,
      --    testCase "annotations have highest parsing priority" annHasPriorityOverArrow,
      testCase "products parse" productsParse,
      appParsing,
      arrParsing,
      depArrParsing
    ]

arrParsing :: TestTree
arrParsing =
  testGroup
    "arr parsing"
    [ testCase "all spaces" $ go "foo : a -> b -> c",
      testCase "indent before ann" $ go "foo\n  : a -> b -> c",
      testCase "indent after ann" $ go "foo :\n  a -> b -> c",
      testCase "indent after first arr" $ go "foo : a ->\n  b -> c",
      testCase "align arrows under ann" $ go "foo\n  : a\n  -> b\n  -> c"
    ]
  where
    go input = do
      actual <- testParse input
      spine actual @?= Ann (Symbol "foo") (Arr Nothing (Symbol "a") (Arr Nothing (Symbol "b") (Symbol "c")))

depArrParsing :: TestTree
depArrParsing =
  testGroup
    "arr parsing"
    [ testCase "all spaces" $ go "foo : (bar : a) -> (fizz : b) -> c",
      testCase "indent before ann" $ go "foo\n  : (bar : a) -> (fizz : b) -> c",
      testCase "indent after ann" $ go "foo :\n  (bar : a) -> (fizz : b) -> c",
      testCase "indent after first arr" $ go "foo : (bar : a) ->\n  (fizz : b) -> c",
      testCase "align arrows under ann" $ go "foo\n  : (bar : a)\n  -> (fizz : b)\n  -> c"
    ]
  where
    go input = do
      actual <- testParse input
      spine actual
        @?= Ann
          (Symbol "foo")
          (Arr (Just "bar") (Symbol "a") (Arr (Just "fizz") (Symbol "b") (Symbol "c")))

appParsing :: TestTree
appParsing =
  testGroup
    "app parsing"
    [ testCase "all spaces" $ go "foo a b c",
      testCase "indent inputs" $ go "foo\n  a b c",
      testCase "indent few inputs" $ go "foo\n  a\n  b c",
      testCase "indent each input" $ go "foo\n  a\n  b\n  c"
    ]
  where
    go input = do
      actual <- testParse input
      spine actual @?= App (Symbol "foo") [Symbol "a", Symbol "b", Symbol "c"]

exampleParses :: IO ()
exampleParses = do
  void $ testParse "let id = \\x -> (\\y -> x) in (id y : t)"

unitParses :: IO ()
unitParses = do
  result <- testParse "()"
  spine result @?= Prod []

parseArrNonDep :: IO ()
parseArrNonDep = do
  result <- testParse "Type -> Type"
  spine result @?= Arr Nothing (Symbol "Type") (Symbol "Type")

parseArrDep :: IO ()
parseArrDep = do
  result <- testParse "(A : Type) -> A -> A"
  spine result @?= Arr (Just "A") (Symbol "Type") (Arr Nothing (Symbol "A") (Symbol "A"))

annParses :: IO ()
annParses = do
  result <- testParse "() : ()"
  spine result @?= (Prod [] `Ann` Prod [])

-- annHasPriorityOverArrow :: IO ()
-- annHasPriorityOverArrow = do
--   result <- testParse "foo : () -> ()"
--   spine result @?= (AST.Symbol "foo" `Ann` (Arr Nothing unitE unitE))

productsParse :: IO ()
productsParse = do
  result <- testParse "(x,y,z)"
  spine result @?= (Prod [Symbol "x", Symbol "y", Symbol "z"])

testParse :: Text -> IO AST
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
