{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.IO.Class
import Data.Text
import Lexer
import Parser
import Runtime
import System.Console.Haskeline
import TypeCheck

main :: IO ()
main = do
  -- Enable altscreen
  putStr "\ESC[?1049h"
  runInputT defaultSettings loop
  -- Disable altscreen, restore previous
  -- terminal state
  putStr "\ESC[?1049l"
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          case lexer "repl" (pack input) of
            Right tokens -> case parse tokens of
              (result : _, _) ->
                liftIO (typeCheck result) >>= \case
                  Right tree -> do
                    ty <- liftIO $ extractValue tree
                    outputStrLn . show $ eval ty
                  Left _ -> outputStrLn "type error!"
              ([], report) -> outputStrLn (show report)
            Left e -> outputStrLn (show e)
          loop
