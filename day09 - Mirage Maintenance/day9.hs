#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci

--------------------------------------
--------------------------------------
----  Day 9:  Mirage Maintenance  ----
--------------------------------------
--------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc -- '.\day9.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day9part1
-- 1584748274

-- *Main> day9part2
-- 1026


-------------
-- Imports --
-------------
import Data.List (foldl')


-------------
-- Program --
-------------
main = day9part1

readSequence :: String -> [Int]
readSequence = map read . words

toDifferenceSequences :: [Int] -> [[Int]]
toDifferenceSequences xs = until end step [xs]
  where step :: [[Int]] -> [[Int]]
        step = (\(prevXs:xss) -> let newXs = zipWith subtract (init prevXs) (tail prevXs) in (newXs:prevXs:xss))
        
        end :: [[Int]] -> Bool
        end = (all (==0) . head)

extrapolate :: [Int] -> Int
extrapolate = sum . map last . toDifferenceSequences

extrapolateBack :: [Int] -> Int
extrapolateBack = foldl' (subtract) 0 . map head . toDifferenceSequences

day9part1 = do
  contents <- readFile "day9 (data).csv"
  print . sum . map extrapolate . map readSequence . lines $ contents

day9part2 = do
  contents <- readFile "day9 (data).csv"
  print . sum . map extrapolateBack . map readSequence . lines $ contents
