#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package linear-1.22

-------------------------------------
-------------------------------------
----  Day 11:  Cosmic Expansion  ----
-------------------------------------
-------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package linear-1.22 -- '.\day11.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day11part1
-- 9521776

-- *Main> day11part2
-- 553224415344


-------------
-- Imports --
-------------
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Linear.V2
import Control.Monad (guard)


-------------
-- Program --
-------------
main = day11part1

expandUniverseString :: String -> String
expandUniverseString = unlines . transpose . expandEmptyRows . transpose . expandEmptyRows . lines
  where expandEmptyRows = concatMap (\row -> if all (=='.') row then replicate 2 row else [row])

expandUniverseStringMore :: String -> String
expandUniverseStringMore = unlines . transpose . expandEmptyRows . transpose . expandEmptyRows . lines
  where expandEmptyRows = concatMap (\row -> if all (=='.') row then replicate 1000000 row else [row])

readStars :: String -> [V2 Int]
readStars inStr = do
    let rows = lines inStr
    (y,row)  <- zip [0..] rows
    (x,char) <- zip [0..] row
    guard $ char == '#'
    return (V2 x y)

taxiCabDistance :: V2 Int -> V2 Int -> Int
taxiCabDistance (V2 x1 y1) (V2 x2 y2) = abs (x2-x1) + abs (y2-y1)

allDistances :: [V2 Int] -> [Int]
allDistances positions = [taxiCabDistance p1 p2 | let indices = take (length positions) [0..], i <- indices, j <- indices, i < j, let p1 = positions !! i, let p2 = positions !! j]

allDistancesWithExpansions :: [V2 Int] -> [Int] -> [Int] -> Int -> [Int]
allDistancesWithExpansions positions expandedRowIndices expandedColIndices expansionAmount = [taxiCabDistance p1 p2 + (numOfExpandedRowsBetween + numOfExpandedColsBetween) * (expansionAmount - 1) | let indices = take (length positions) [0..], i <- indices, j <- indices, i < j, let p1 = positions !! i, let p2 = positions !! j, let (V2 x1 y1) = p1, let (V2 x2 y2) = p2, let minX = min x1 x2, let maxX = max x1 x2, let minY = min y1 y2, let maxY = max y1 y2, let numOfExpandedRowsBetween = length . filter (\y' -> y' <= maxY) . filter (\y' -> y' >= minY) $ expandedRowIndices, let numOfExpandedColsBetween = length . filter (\x' -> x' <= maxX) . filter (\x' -> x' >= minX) $ expandedColIndices]

day11part1 = do
  contents <- readFile "day11 (data).csv"
  print . sum . allDistances . readStars . expandUniverseString $ contents

day11part2 = do
  contents <- readFile "day11 (data).csv"
  let stars = readStars contents
      emptyRowIndices = catMaybes . map (\(i,row) -> if all (=='.') row then Just i else Nothing) . zip [0..] . lines $ contents
      emptyColIndices = catMaybes . map (\(i,row) -> if all (=='.') row then Just i else Nothing) . zip [0..] . transpose . lines $ contents
      
      distances :: [Int]
      distances = allDistancesWithExpansions stars emptyRowIndices emptyColIndices 1000000
  print . sum $ distances