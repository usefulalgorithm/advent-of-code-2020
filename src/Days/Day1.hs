module Days.Day1 (day1) where

import           Days.ReadPuzzle

day1 :: IO ()
day1 = do
  input <- readPuzzle 1
  putStr "First: "
  print $ first input
  putStr "Second: "
  print $ second input

first :: String -> Int
first input =
  head $ [ x * y | x <- nums, y <- nums, x /= y, x + y == 2020 ]
  where nums = read <$> words input

second :: String -> Int
second input =
  head $ [ x * y * z | x <- nums, y <- nums, z <- nums, x /= y, y /= z, x /= z, x + y + z == 2020 ]
  where nums = read <$> words input
