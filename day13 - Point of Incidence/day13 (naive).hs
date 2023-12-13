#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22 --package split-0.2.3.5 --package parallel-3.2.2.0

---------------------------------------
---------------------------------------
----  Day 13:  Point of Incidence  ----
---------------------------------------
---------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package linear-1.22 -- -threaded -O2 '.\day13.hs'
-}

------------
-- Output --
------------
-- *Main> day13part1
-- 

-- *Main> day13part2
-- 


-------------
-- Imports --
-------------
import Data.List (foldl', nub, sort, transpose, isPrefixOf, isSuffixOf, findIndex, findIndices, intercalate, reverse)
import Data.List.Split (splitOn, chunksOf)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybeToList, catMaybes)
import Linear.V2
import Control.Monad (guard)
import Debug.Trace (trace)
import Control.Parallel.Strategies
import Data.Bits


-------------
-- Program --
-------------
main = day13part1

data RockPattern = RockPattern {rockPatternRows :: [String], rockPatternRowLength :: Int, rockPatternRowAmount :: Int} deriving (Show)
newtype RockPatternString = RockPatternString {fromRockPatternString :: String} deriving (Show)

readRockPatternStrings :: String -> [RockPatternString]
readRockPatternStrings = map RockPatternString . splitOn "\n\n"

readRockPattern :: String -> RockPattern
readRockPattern inStr = RockPattern pattern rowLength rowAmount
  where rows = lines inStr
        
        rowAmount = length rows
        rowLength = length (head rows)
        pattern = rows

findHorizontalReflectionIndices :: RockPatternString -> [Int]
findHorizontalReflectionIndices (RockPatternString s) = findIndices (==True) $ matchesBetween rowLength rowAmount (rockPatternRows rockPatternDn) (rockPatternRows rockPatternUp)
  where rockPatternDn = readRockPattern . id                                    $ s
        rockPatternUp = readRockPattern . unlines . reverse             . lines $ s
        
        rowAmount = rockPatternRowAmount rockPatternDn
        rowLength = rockPatternRowLength rockPatternDn

findVerticalReflectionIndices :: RockPatternString -> [Int]
findVerticalReflectionIndices (RockPatternString s) = findIndices (==True) $ matchesBetween rowLength rowAmount (rockPatternRows rockPatternLt) (rockPatternRows rockPatternRt)
  where rockPatternLt = readRockPattern . unlines .           transpose . lines $ s
        rockPatternRt = readRockPattern . unlines . reverse . transpose . lines $ s
        
        rowAmount = rockPatternRowAmount rockPatternLt
        rowLength = rockPatternRowLength rockPatternLt

truncateBits :: Int -> Int -> Int
truncateBits digits x = x .&. ((1 `shiftL` digits) - 1)

matchesBetween :: Int -> Int -> [String] -> [String] -> [Bool]
matchesBetween rowLength numOfRows rocks1 rocks2 = left ++ right
  where total = numOfRows
        
        left  = [drop i rocks1 `isPrefixOf` rocks2 | i <- [0 .. (numOfRows-2)]]
        right = [take i rocks2 `isSuffixOf` rocks1 | i <- [2 .. (numOfRows-1)]]

toBin :: Int -> Int -> String
toBin bitCount n = [if n `testBit` i then '#' else '.' | i <- [0..(bitCount-1)]]

day13part1 = do
  contents <- readFile "day13 (example).csv"
  let colSum = map findVerticalReflectionIndices . readRockPatternStrings $ contents
  let rowSum = map findHorizontalReflectionIndices . readRockPatternStrings $ contents
  print (rowSum, colSum)
