module Parse where

import           Control.Monad
import           Data.Tree

printLineWithNumbers file = do
  forM_ (zip [1 :: Int ..] (lines file))
    $ \(n, line) -> putStrLn $ (show n) <> ": " <> line

example = "hello\n  world\nI'm Jeff"
answer = [Node "hello" [Node "  world" []], Node "I'm Jeff" []]

getIndentation :: String -> Int
getIndentation = length . takeWhile (' ' ==)

block :: [String] -> Forest String
block [] = []
block (next : rest) =
  let n                = getIndentation next
      (subGroup, more) = span ((n <) . getIndentation) rest
      newGroup         = Node next (block $ drop n <$> subGroup)
      otherGroups      = block more
  in  newGroup : otherGroups

removeEmptyLines :: [String] -> [String]
removeEmptyLines = filter (not . isEmpty)
 where
  isEmpty []           = True
  isEmpty (' ' : rest) = isEmpty rest
  isEmpty _            = False

data AST
  = App AST AST
  | Num Int
  | Sym String

apple = "someFunc 1 2 3"
bake = ["someFunc", "1", "2", "3"]
cake = Sym "someFunc" `App` Num 1 `App` Num 2 `App` Num 3

parseApp :: String -> String -> [String]
parseApp [] acc = [acc]
parseApp (c:rest) acc =
  if c == ' '
    then acc : parseApp rest "" 
    else parseApp rest (acc <> [c])

  -- 1. Char for whitespace
  -- 2. If there is whitespace, look next to see if something after
  -- 3. If no whitespace, keep adding to existing element

test :: IO ()
test = do
  putStrLn "Starting Parsing..."
  file <- readFile "examples/Example1.jy"
  let ls     = removeEmptyLines $ lines file
  let groups = block ls
  forM_ groups (putStrLn . drawTree)
