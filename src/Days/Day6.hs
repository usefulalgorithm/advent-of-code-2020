module Days.Day6 (day6) where

import           Data.List       (nub)
import           Days.ReadPuzzle

day6 :: IO ()
day6 = do
  input <- readPuzzle 6
  putStr "First: "
  print $ first input
  putStr "Second: "
  print $ second input

toGroups :: String -> [String]
toGroups = map unwords . foldl (\acc s -> if null s then []:acc else (s:head acc): tail acc) [[]] . lines

first :: String -> Int
first = sum . map (length . nub . concat . words) . toGroups

commons :: [String] -> Int
commons ls = length $ filter (and . flip map ls . elem) "abcdefghijklmnopqrstuvwxyz"

second :: String -> Int
second = sum . map (commons . words) . toGroups
