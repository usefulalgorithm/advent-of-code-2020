module Days.Day5 (day5) where

import           Days.ReadPuzzle

day5 :: IO ()
day5 = do
  input <- readPuzzle 5
  putStr "First: "
  print $ first input
  putStr "Second: "
  print $ second input

binaryToBool :: Char -> Bool
binaryToBool 'F' = False
binaryToBool 'B' = True
binaryToBool 'L' = False
binaryToBool 'R' = True
binaryToBool _   = False

toPartition :: String -> [[(Int, Bool)]]
toPartition x = map (zip [0..] . reverse . map binaryToBool) $ [take 7 x, drop 7 x]

ziplistToNum :: [(Int, Bool)] -> Int
ziplistToNum = sum . map (\(n, p) -> if p then 2 ^ n else 0)

toSeatID :: [Int] -> Int
toSeatID xs = (xs !! 0) * 8 + (xs !! 1)

toSeatIDs :: String -> Int
toSeatIDs = toSeatID . map ziplistToNum . toPartition

first :: String -> Int
first = maximum . map toSeatIDs . lines

second :: String -> Int
second input = last $ filter (not . flip elem (map toSeatIDs . lines $ input)) [8..(first input)]
