{-# LANGUAGE LambdaCase #-}

module Main where

{-
import Control.Monad.IO.Class
import Data.Text
import Lexer
import Parser
import Runtime
import Runtime.Types
import System.Console.Haskeline
import TypeCheck
import TypeCheck.Types

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
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "pasta> "
      case minput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just input -> do
          case lexer "repl" (pack input) of
            Right tokens -> case parse tokens of
              (result : _, _) ->
                liftIO (typeCheck result defaultTyCheckSt noSetup) >>= \case
                  Right tree -> do
                    let val = dropTypes tree
                    outputStrLn $ displayRtVal $ eval val
                  Left _ -> outputStrLn "type error!"
              ([], report) -> outputStrLn (show report)
            Left e -> outputStrLn (show e)
          loop
-}
main = pure ()
