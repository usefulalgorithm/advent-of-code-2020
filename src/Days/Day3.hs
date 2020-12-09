module Days.Day3 (day3) where

import           Data.List       (transpose)
import           Data.List.Split (chunksOf)
import           Days.ReadPuzzle

day3 :: IO ()
day3 = do
  input <- readPuzzle 3
  putStr "First: "
  print $ first input
  putStr "Second: "
  print $ second input

toGrid :: Int -> String -> Char
toGrid i line = line !! (mod i (length line))

go :: [Int] -> [String] -> Int
go slope forest = length . filter (== '#') . map (uncurry toGrid) $ zip slope forest

first :: String -> Int
first input =  go [0,3..] $ lines input

getForests :: [String] -> [[String]]
getForests forest = (head . transpose $ chunksOf 2 forest):(replicate 4 $ forest)

slopes :: [[Int]]
slopes = [[0,1..],[0,1..],[0,3..],[0,5..],[0,7..]]

second :: String -> Int
second input = foldl (*) 1 . map (uncurry go) . zip slopes . getForests $ lines input
