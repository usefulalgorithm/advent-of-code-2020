module Days.Day7 (day7) where

import           Days.ReadPuzzle

day7 :: IO ()
day7 = do
  input <- readPuzzle 7
  putStr "First: "
  print $ first input
  putStr "Second: "
  print $ second input

first :: String -> Int
first _ = 0

second :: String -> Int
second _ = 0
