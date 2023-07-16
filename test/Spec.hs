{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import AST.Expr
import qualified AST.Expr as AST
import AST.LocTree
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logic
import Data.List.NonEmpty
import System.Environment
import qualified Test.Parsing as Parsing
import qualified Test.Runtime as Runtime
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  args <- getArgs
  withArgs ("--hide-successes" : args) $ defaultMain $ tests
  where
    tests = testGroup "compiler" [Parsing.tests, Runtime.tests]
