{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( module AST.Transform,
    module AST.Core,
    module TypeCheck.Typed,
    module Eval.WHNF,
    module Eval.Normal,
    module Repl,
    module Parser,
    module Parser.Lexer,
    module AST.Surface,
  )
where

import AST.Core
import AST.Surface
import AST.Transform
import Display
import Eval.Normal
import Eval.WHNF
import Logic
import Parser
import Parser.Lexer
import RIO.Text (unpack)
import Repl
import TypeCheck.Typed

test :: IO ()
test = do
  case parse "(x : a -> a) (y : b)" of
    Right exp -> do
      print $ exp
      let input = desugar exp
      let st = initCheckST
      runTypeCheck st input >>= \case
        (Info (MkTypeMerge _ _ result) : _) -> print $ unpack $ display $ renderHoles result
        _ -> print "No Type results"
    Left e -> putStrLn $ displayParseErr e
