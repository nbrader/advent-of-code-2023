#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22

-------------------------------------
-------------------------------------
----  Day 11:  Cosmic Expansion  ----
-------------------------------------
-------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package linear-1.22 -- '.\day11.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day11part1
-- 

-- *Main> day11part2
-- 


-------------
-- Imports --
-------------
import Data.List (foldl', nub, sort, transpose)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybeToList)
import Linear.V2
import Control.Monad (guard)
import Debug.Trace (trace)


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

allDistances :: [V2 Int] -> [Int]
allDistances positions = [abs (x2-x1) + abs (y2-y1) | let indices = take (length positions) [0..], i <- indices, j <- indices, i < j, let (V2 x1 y1) = positions !! i, let (V2 x2 y2) = positions !! j]

day11part1 = do
  contents <- readFile "day11 (data).csv"
  print . sum . allDistances . readStars . expandUniverseString $ contents

day11part2 = do
  contents <- readFile "day11 (data).csv"
  print . sum . allDistances . readStars . expandUniverseStringMore $ contents