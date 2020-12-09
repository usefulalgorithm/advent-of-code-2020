module Days.Day4 (day4) where

import           Days.ReadPuzzle

day4 :: IO ()
day4 = do
  input <- readPuzzle 4
  putStr "First: "
  print $ first input
  putStr "Second: "
  print $ second input

first :: String -> Int
first _ = 0

second :: String -> Int
second _ = 0
