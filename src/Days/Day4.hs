module Days.Day4 (day4) where

import           Data.Char       (isAlpha)
import           Data.List.Split (splitOn)
import           Data.Set        as S (Set, fromList)
import           Days.ReadPuzzle

day4 :: IO ()
day4 = do
  input <- readPuzzle 4
  putStr "First: "
  print $ first input
  putStr "Second: "
  print $ second input

toPassport :: String -> [String]
toPassport = map unwords . foldl (\acc s -> if null s then []:acc else (s:head acc): tail acc) [[]] . lines

okFields :: [String]
okFields = ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]

allFields :: [String]
allFields = "cid" : okFields

validFieldsSets :: [S.Set String]
validFieldsSets = [ S.fromList okFields, S.fromList allFields ]

isValid :: String -> Bool
isValid = flip elem validFieldsSets . S.fromList . map (takeWhile (/= ':')) . words

first :: String -> Int
first = length . filter isValid . toPassport

checkValue :: String -> Bool
checkValue = uncurry checkValue' . (\x -> (head x, (head $ tail x))) . splitOn ":"

checkValue' :: String -> String -> Bool
checkValue' "hgt" x = checkHgt val unit
  where
    val = read $ takeWhile (not . isAlpha) x
    unit = dropWhile (not . isAlpha) x
    checkHgt :: Int -> String -> Bool
    checkHgt n "cm" = n >= 150 && n <= 193
    checkHgt n "in" = n >= 59 && n <= 76
    checkHgt _ _    = False
checkValue' "hcl" x = ('#' == head x) && (6 == length n) && (all (flip elem "0123456789abcdef") n)
  where
    n = tail x
checkValue' "ecl" x = flip elem (words "amb blu brn gry grn hzl oth") x
checkValue' "pid" x | 9 /= length x = False
checkValue' f v = checkValue'' f $ read v

checkValue'' :: String -> Int -> Bool
checkValue'' "byr" x = x >= 1920 && x <= 2002
checkValue'' "iyr" x = x >= 2010 && x <= 2020
checkValue'' "eyr" x = x >= 2020 && x <= 2030
checkValue'' "pid" x = x >= 0 && x < 1000000000
checkValue'' "cid" _ = True
checkValue'' _ _     = False

isValidValues :: String -> Bool
isValidValues = and . map checkValue . words

second :: String -> Int
second = length . filter isValidValues . filter isValid . toPassport
