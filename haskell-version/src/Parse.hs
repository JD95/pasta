module Parse where

import Control.Monad

printLineWithNumbers file = do
  forM_ (zip [1..] (lines file)) $ \(n, line) ->
    putStrLn $ (show n) <> ": " <> line

example = "hello\n"
       <> "  world\n"
       <> "I'm Jeff"

answer = [["hello", "  world"], ["I'm Jeff"]]

isIndented :: String -> Bool
isIndented line =
  case line of
    (' ':_) -> True 
    _ -> False 

block :: [String] -> [[String]]
block [] = []
block (next:rest) =
  if not (isIndented next)
    then let (subGroup, more) = span isIndented rest
             newGroup = next : subGroup
             otherGroups = block more 
         in newGroup : otherGroups 
    else error "Bad indentation"

removeEmptyLines :: [String] -> [String]
removeEmptyLines lines = filter (not . isEmpty) lines where
  isEmpty [] = True
  isEmpty (' ':rest) = isEmpty rest
  isEmpty _ = False

test :: IO ()
test = do
  putStrLn "Starting Parsing..."
  file <- readFile "examples/Example1.jy"
  let ls = removeEmptyLines $ lines file
  let groups = block ls
  forM_ groups print
