#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22 --package fastmemo-0.1.1

--------------------------------------
--------------------------------------
----  Day 9:  Mirage Maintenance  ----
--------------------------------------
--------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22 -- '.\day9.hs' -O2
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
import Data.Char (isDigit, isSpace, isAlphaNum)
import Data.List (sort, tails, isPrefixOf, groupBy, find, nub, foldl', group, sort, intercalate)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (fromJust, fromMaybe)
import Data.Map as M hiding (map, filter, take, drop, foldl', null)
import Debug.Trace (trace)
import Data.Tuple (swap)
import Data.Function (on, fix)
import Data.Function.FastMemo (memoize)
import Data.Ord (comparing)


-------------
-- Program --
-------------
main = day9part1

readSeqence :: String -> [Int]
readSeqence = map read . words

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
  contents <- readFile "day9 (data 3).csv"
  print . sum . map extrapolate . map readSeqence . lines $ contents

day9part2 = do
  contents <- readFile "day9 (data 3).csv"
  print . sum . map extrapolateBack . map readSeqence . lines $ contents
