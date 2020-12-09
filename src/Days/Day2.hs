module Days.Day2 (day2) where

import           Days.ReadPuzzle

day2 :: IO ()
day2 = do
  input <- readPuzzle 2
  putStr "First: "
  print $ first input
  putStr "Second: "
  print $ second input

first :: String -> Int
first input = length . filter isValid $ words <$> lines input
  where isValid (ts:c:pw:_) = (\x -> x >= read (takeWhile (/= '-') ts) && x <= (read (tail (dropWhile (/= '-') ts)))) . length $ filter (== (head c)) pw
        isValid _ = False

second :: String -> Int
second input =
  let
    isValid (ts:cs:pw:_) = (\(i, j) -> ((pw !! (i-1)) == c) /= ((pw !! (j-1)) == c)) $ parsePassword ts
      where c = head cs
    isValid _ = False
    parsePassword ts = (read (takeWhile (/= '-') ts), read (tail (dropWhile (/= '-') ts)))
  in
    length . filter isValid $ words <$> lines input
