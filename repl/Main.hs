{-# LANGUAGE LambdaCase #-}

module Main where

import AST.LocTree
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Parsing.Desugar
import Parsing.Grammar
import Parsing.Lexer
import Refinement
import System.Console.Haskeline

data ReplException
  = LexPhaseErr LexError

main :: IO ()
main = do
  -- Enable altscreen
  putStr "\ESC[?1049h"
  putStrLn "Welcome to the Pasta!"
  runInputT defaultSettings loop
  -- Disable altscreen, restore previous
  -- terminal state
  putStr "\ESC[?1049l"
  where
    lexingPhase input =
      case lexer "repl" input of
        Left e -> error "Lex error!"
        Right src -> pure src

    parsingPhase tokens =
      case parse tokens of
        Left e -> error "Parse error!"
        Right src -> pure $ desugar src

    refinementPhase src =
      liftIO (refinement src (Env [])) >>= \case
        Right result -> pure result
        Left e -> error "Refinement Error!"

    realizationPhase result =
      liftIO $ realization result (Env [])

    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "🧆> "
      case minput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just input -> do
          lexingPhase (Text.pack input)
            >>= parsingPhase
            >>= refinementPhase
            >>= realizationPhase
          loop
