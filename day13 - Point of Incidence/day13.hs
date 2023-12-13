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
import Data.List (foldl', nub, sort, transpose, isPrefixOf, findIndex, findIndices, intercalate, reverse)
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

data RockPattern = RockPattern {rockPatternInt :: Int, rockPatternRowLength :: Int, rockPatternRowAmount :: Int} deriving (Show)
newtype RockPatternString = RockPatternString {fromRockPatternString :: String} deriving (Show)

readRockPatternStrings :: String -> [RockPatternString]
readRockPatternStrings = map RockPatternString . splitOn "\n\n"

readRockPattern :: String -> RockPattern
readRockPattern inStr = RockPattern pattern rowLength rowAmount
  where rows = lines inStr
        
        rowAmount = length rows
        rowLength = length (head rows)
        pattern = sum $ zipWith (\i digit -> digit*2^i) [0..] . map (\c -> if c == '#' then 1 else 0) $ concat rows

allPatternDirs :: RockPatternString -> (RockPattern, RockPattern, RockPattern, RockPattern)
allPatternDirs (RockPatternString s) = (rockPatternDn, rockPatternUp, rockPatternLt, rockPatternRt)
  where rockPatternDn = readRockPattern . id                                    $ s
        rockPatternUp = readRockPattern . unlines . reverse             . lines $ s
        rockPatternLt = readRockPattern . unlines .           transpose . lines $ s
        rockPatternRt = readRockPattern . unlines . reverse . transpose . lines $ s

-- printAllDirs (rockPatternDn, rockPatternUp, rockPatternLt, rockPatternRt) = do
    -- print rockPatternDn
    -- print rockPatternUp
    -- print rockPatternLt
    -- print rockPatternRt
    -- putStrLn ""

-- day13part1 = do
  -- contents <- readFile "day13 (example).csv"
  -- mapM_ printAllDirs . map allPatternDirs . readRockPatternStrings $ contents

findHorizontalReflectionIndices :: RockPatternString -> [Int]
findHorizontalReflectionIndices (RockPatternString s) = findIndices (==0) $ allXorsBetween rowLength rowAmount (rockPatternInt rockPatternDn) (rockPatternInt rockPatternUp)
  where rockPatternDn = readRockPattern . id                                    $ s
        rockPatternUp = readRockPattern . unlines . reverse             . lines $ s
        
        rowAmount = rockPatternRowAmount rockPatternDn
        rowLength = rockPatternRowLength rockPatternDn

findVerticalReflectionIndices :: RockPatternString -> [Int]
findVerticalReflectionIndices (RockPatternString s) = findIndices (==0) $ allXorsBetween rowLength rowAmount (rockPatternInt rockPatternLt) (rockPatternInt rockPatternRt)
  where rockPatternLt = readRockPattern . unlines .           transpose . lines $ s
        rockPatternRt = readRockPattern . unlines . reverse . transpose . lines $ s
        
        rowAmount = rockPatternRowAmount rockPatternLt
        rowLength = rockPatternRowLength rockPatternLt

truncateBits :: Int -> Int -> Int
truncateBits digits x = x .&. ((1 `shiftL` digits) - 1)

allXorsBetween :: Int -> Int -> Int -> Int -> [Int]
allXorsBetween rowLength numOfRows rockInt1 rockInt2 = left ++ right
  where left  = [(chop $ rockInt1) `xor` shiftR rockInt2 delta | i <- [(numOfRows-2), (numOfRows-3) .. 1], let delta = i*rowLength, let chop = truncateBits ((numOfRows - i)*rowLength)]
        right = [shiftR rockInt1 delta `xor` (chop $ rockInt2) | i <- [0..(numOfRows-2)],                  let delta = i*rowLength, let chop = truncateBits ((numOfRows - i)*rowLength)]

-- allEvenXorsBetween :: Int -> Int -> Int -> Int -> [Int]
-- allEvenXorsBetween rowLength numOfRows rockInt1 rockInt2 = left ++ right
  -- where left  = [(chop $ rockInt1) `xor` shiftR rockInt2 delta | i <- [(numOfRows-2), (numOfRows-3) .. 1], let height = (numOfRows - i), height `mod` 2 == 0, let delta = i*rowLength, let chop = truncateBits (height*rowLength)]
        -- right = [shiftR rockInt1 delta `xor` (chop $ rockInt2) | i <- [0..(numOfRows-2)],                  let height = (numOfRows - i), height `mod` 2 == 0, let delta = i*rowLength, let chop = truncateBits (height*rowLength)]

toBin :: Int -> Int -> String
toBin bitCount n = [if n `testBit` i then '#' else '.' | i <- [0..(bitCount-1)]]

day13part1 = do
  contents <- readFile "day13 (example).csv"
  let colSum = map findVerticalReflectionIndices . readRockPatternStrings $ contents
  let rowSum = map findHorizontalReflectionIndices . readRockPatternStrings $ contents
  print (rowSum, colSum)

-- Given allXorsBetween gives index for a reflection about the line between rows or on a row starting between the first two rows and ending between the last two rows, we should get the following:
-- testStr1 = unlines [".  #  #  .  .  #  #  .  .",
--                                                  0
--                     ".  #  .  #  #  .  #  .  .", 1
--                                                  2
--                     "#  .  .  .  .  .  .  #  .", 3
--                                                  4 <---- Reflection
--                     "#  .  .  .  .  .  .  #  .", 5
--                                                  6
--                     ".  #  .  #  #  .  #  .  .", 7
--                                                  8
--                     ".  #  #  .  .  #  #  .  .", 9
--                                                  10
--                     ".  #  .  #  #  .  #  .  ."] 
--                         1  3  5  7  9  11 13     
--                       0  2  4  6  8  10 12 14    
--                                ^                    
--                                |                    
--                                |                    
--                                |                    
--                                |                    
--                                Reflection           
-- 
--  ([[4]],[[6]])
-- 
-- The following code does not match this expectation:
--  ([[5,10]],[[7]])
--
-- And the other inputs below also come out wrong
--
tests = do
  let rockPatternStrings = readRockPatternStrings (intercalate "\n\n" [testStr1,testStr2,testStr3])
  let colSum = map (map (+1)) . map findVerticalReflectionIndices $ rockPatternStrings
  let rowSum = map (map (+1)) . map findHorizontalReflectionIndices $ rockPatternStrings
  print (rowSum, colSum)

testStr1 = unlines [".##..##..",
                    ".#.##.#..",
                    "#......#.",
                    "#......#.",
                    ".#.##.#..",
                    ".##..##..",
                    ".#.##.#.."]

testStr2 = unlines ["..##..##..",
                    "..#.##.#..",
                    ".#......#.",
                    ".#......#.",
                    "..#.##.#..",
                    "..##..##..",
                    "..#.##.#.."]

testStr3 = unlines ["...##..##..",
                    "...#.##.#..",
                    "..#......#.",
                    "..#......#.",
                    "...#.##.#..",
                    "...##..##..",
                    "...#.##.#.."]
