#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22 --package fastmemo-0.1.1

-------------------------------
-------------------------------
----  Day 6:  Wait For It  ----
-------------------------------
-------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22 -- '.\day6.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day6part1
-- 2065338

-- *Main> day6part2
-- 34934171


-------------
-- Imports --
-------------
import Data.Char (isDigit)
import Data.List (sort, tails, isPrefixOf, groupBy, find, nub, foldl')
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)
import Data.Tuple (swap)
import Data.Function (on, fix)
import Data.Function.FastMemo (memoize)


-------------
-- Program --
-------------
main = day6part1

data Race = Race {raceTimeLimits :: Int, raceRecordDistance :: Int} deriving (Show)

readRaces :: String -> [Race]
readRaces inStr = zipWith Race timeLimits recordDistances
  where (timeLimitsStr, after1) = break (=='\n') (drop (length "Time:      ") $ inStr)
        recordDistancesStr      =                 drop (length "Distance:  ") $ after1
        
        timeLimits      = map read $ words timeLimitsStr
        recordDistances = map read $ words recordDistancesStr

readAsSingleRace :: String -> Race
readAsSingleRace inStr = Race timeLimit recordDistance
  where (timeLimitsStr, after1) = break (=='\n') (drop (length "Time:      ") $ inStr)
        recordDistancesStr      =                 drop (length "Distance:  ") $ after1
        
        timeLimit      = read $ concat $ words timeLimitsStr
        recordDistance = read $ concat $ words recordDistancesStr

numberOfWinningOptions :: Race -> Int
numberOfWinningOptions (Race timeLimits recordDistances) = length $ filter (> recordDistances) $ map distance pressTimes
  where distance pressTime = pressTime*(timeLimits - pressTime)
        pressTimes = take (timeLimits+1) [0..]

day6part1 = do
  contents <- readFile "day6 (data).csv"
  print $ product $ map numberOfWinningOptions $ readRaces $ contents

day6part2 = do
  contents <- readFile "day6 (data).csv"
  print $ numberOfWinningOptions $ readAsSingleRace $ contents