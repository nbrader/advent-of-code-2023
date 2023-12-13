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


--------


-- findHorizontalReflectionIndices :: RockPatternString -> [Int]
-- findHorizontalReflectionIndices (RockPatternString s) = findIndices (==0) $ allXorsBetween (rockPatternRowLength rockPatternDn) (rockPatternRowAmount rockPatternDn) (rockPatternInt rockPatternDn) (rockPatternInt rockPatternUp)
  -- where rockPatternDn = readRockPattern . id                                    $ s
        -- rockPatternUp = readRockPattern . unlines . reverse             . lines $ s

-- findVerticalReflectionIndices :: RockPatternString -> [Int]
-- findVerticalReflectionIndices (RockPatternString s) = findIndices (==0) $ allXorsBetween (rockPatternRowLength rockPatternLt) (rockPatternRowAmount rockPatternLt) (rockPatternInt rockPatternLt) (rockPatternInt rockPatternRt)
  -- where rockPatternLt = readRockPattern . unlines .           transpose . lines $ s
        -- rockPatternRt = readRockPattern . unlines . reverse . transpose . lines $ s

-- truncateBits :: Int -> Int -> Int
-- truncateBits digits x = x .&. ((1 `shiftL` digits) - 1)

-- allXorsBetween :: Int -> Int -> Int -> Int -> [Int]
-- -- allXorsBetween rowLength rockInt1 rockInt2 = undefined
-- allXorsBetween rowLength numOfRows rockInt1 rockInt2 = left ++ right
  -- where left  = [chop $ rockInt1 `xor` shiftR rockInt2 delta | i <- [(numOfRows-2), (numOfRows-3) .. 1], let delta = i*rowLength, let chop = truncateBits ((numOfRows - i)*rowLength)]
        -- right = [chop $ shiftR rockInt1 delta `xor` rockInt2 | i <- [0..(numOfRows-2)], let delta = i*rowLength, let chop = truncateBits ((numOfRows - i)*rowLength)]

-- day13part1 = do
  -- contents <- readFile "day13 (example).csv"
  -- mapM_ print . map findHorizontalReflectionIndices . readRockPatternStrings $ contents
  -- mapM_ print . map findVerticalReflectionIndices . readRockPatternStrings $ contents



--------


-- findHorizontalReflectionIndices :: RockPatternString -> [Int]
findHorizontalReflectionIndices (RockPatternString s) = map (\(l,r) -> (chunksOf rowLength $ toBin (rowLength * rowAmount) l, chunksOf rowLength $ toBin (rowLength * rowAmount) r)) $ allXorsBetween rowLength rowAmount (rockPatternInt rockPatternDn) (rockPatternInt rockPatternUp)
  where rockPatternDn = readRockPattern . id                                    $ s
        rockPatternUp = readRockPattern . unlines . reverse             . lines $ s
        
        rowAmount = rockPatternRowAmount rockPatternDn
        rowLength = rockPatternRowLength rockPatternDn

-- findVerticalReflectionIndices :: RockPatternString -> [Int]
findVerticalReflectionIndices (RockPatternString s) = map (\(l,r) -> (chunksOf rowLength $ toBin (rowLength * rowAmount) l, chunksOf rowLength $ toBin (rowLength * rowAmount) r)) $ allXorsBetween rowLength rowAmount (rockPatternInt rockPatternLt) (rockPatternInt rockPatternRt)
  where rockPatternLt = readRockPattern . unlines .           transpose . lines $ s
        rockPatternRt = readRockPattern . unlines . reverse . transpose . lines $ s
        
        rowAmount = rockPatternRowAmount rockPatternLt
        rowLength = rockPatternRowLength rockPatternLt

truncateBits :: Int -> Int -> Int
truncateBits digits x = x .&. ((1 `shiftL` digits) - 1)

allXorsBetween :: Int -> Int -> Int -> Int -> [(Int,Int)]
-- allXorsBetween rowLength rockInt1 rockInt2 = undefined
allXorsBetween rowLength numOfRows rockInt1 rockInt2 = left ++ right
  where left  = [(chop $ rockInt1, shiftR rockInt2 delta) | i <- [(numOfRows-2), (numOfRows-3) .. 1], let delta = i*rowLength, let chop = truncateBits ((numOfRows - i)*rowLength)]
        right = [(shiftR rockInt1 delta, chop $ rockInt2) | i <- [0..(numOfRows-2)],                  let delta = i*rowLength, let chop = truncateBits ((numOfRows - i)*rowLength)]

toBin :: Int -> Int -> String
toBin bitCount n = [if n `testBit` i then '#' else '.' | i <- [0..(bitCount-1)]]

day13part1 = do
  contents <- readFile "day13 (example).csv"
  mapM_ print . findHorizontalReflectionIndices . head . readRockPatternStrings $ contents
  print ""
  mapM_ print . findVerticalReflectionIndices . head . readRockPatternStrings $ contents
  -- mapM_ (mapM_ (\(l,r) -> mapM_ print l >> putStrLn "" >> mapM_ print r >> putStrLn "")) . map findHorizontalReflectionIndices . readRockPatternStrings $ contents
