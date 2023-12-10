#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci

-------------------------------
-------------------------------
----  Day 6:  Wait For It  ----
-------------------------------
-------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc -- '.\day6.hs' -O2
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

-- pressTime*pressTime - timeLimit*pressTime + recordDistances > 0
numberOfWinningOptions' :: Race -> Int
numberOfWinningOptions' (Race timeLimit recordDistances) = if halfInteger then 2*floor (pressTimeMidRadius+0.5) else 2*(floor pressTimeMidRadius) + 1
  where recordDistances' = fromIntegral recordDistances
        timeLimit' = fromIntegral timeLimit
        
        -- pressTimeMidPoint = timeLimit'/2
        pressTimeMidRadius = (sqrt ((timeLimit'^2 - 4*recordDistances')))/2
        
        halfInteger = timeLimit `mod` 2 == 1

day6part1 = do
  contents <- readFile "day6 (data).csv"
  print $ product $ map numberOfWinningOptions' $ readRaces $ contents

day6part2 = do
  contents <- readFile "day6 (data).csv"
  print $ numberOfWinningOptions' $ readAsSingleRace $ contents