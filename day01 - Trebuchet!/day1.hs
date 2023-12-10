#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci

------------------------------
------------------------------
----  Day 1:  Trebuchet!  ----
------------------------------
------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc -- '.\day1.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day1part1
-- 54708

-- *Main> day1part2
-- 54087


-------------
-- Imports --
-------------
import Data.Char (isDigit)
import Data.List (tails, isPrefixOf)
import Data.Map as M hiding (map, filter)
import Data.Maybe (fromJust)


-------------
-- Program --
-------------
main = day1part2

day1part1 = do
  contents <- readFile "day1 (data).csv"
  let total = sum . map read . map (\xs -> [head xs, last xs])
                  . map (filter isDigit) . lines $ contents
  print $ total

digitWords :: M.Map String Int
digitWords
    = M.fromList [
        ("0",0), ("1",1), ("2",2), ("3",3), ("4",4),
        ("5",5), ("6",6), ("7",7), ("8",8),("9",9),
        ("zero",0), ("one",1), ("two",2), ("three",3), ("four",4),
        ("five",5), ("six",6), ("seven",7), ("eight",8), ("nine",9)
        ]

lineToNum :: String -> Int
lineToNum xs = read . concatDigits . firstAndLast $ allDigitsInLine
  where allDigitsInLine :: [Int]
        allDigitsInLine
            = [fromJust (M.lookup key digitWords) |
                end <- tails xs,
                key <- M.keys digitWords,
                key `isPrefixOf` end]
        
        firstAndLast :: [a] -> [a]
        firstAndLast xs = [head xs, last xs]
        
        concatDigits :: [Int] -> String
        concatDigits xs = concat . map show $ xs

day1part2 = do
  contents <- readFile "day1 (data).csv"
  let total = sum . map lineToNum . lines $ contents
  print $ total