#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22 --package split-0.2.3.5

--------------------------------
--------------------------------
----  Day 12:  Hot Springs  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package linear-1.22 -- '.\day12.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day12part1
-- 

-- *Main> day12part2
-- 


-------------
-- Imports --
-------------
import Data.List (foldl', nub, sort, transpose, isPrefixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybeToList, catMaybes)
import Linear.V2
import Control.Monad (guard)
import Debug.Trace (trace)


-------------
-- Program --
-------------
main = day12part1

data SpringRow = SpringRow {srConditionsStr :: String, srDamagedRuns :: [Int]} deriving (Show)

readSpringRow :: String -> SpringRow
readSpringRow = (\[conditionsStr,groupsStr] -> SpringRow conditionsStr (map read $ splitOn "," groupsStr)) . words

arrangements :: SpringRow -> [String]
arrangements (SpringRow ""            []                      ) = [""]
arrangements (SpringRow ""            (damagedRun:damagedRuns)) = []
arrangements (SpringRow conditionsStr [])
    | all (\c -> c == '?' || c == '.') conditionsStr = [allAsUndamaged]
    | otherwise                                      = []
  where allAsUndamaged = replicate (length conditionsStr) '.'
arrangements (SpringRow conditionsStr (damagedRun:damagedRuns))
    | damagedPrefix `isPrefixOf` conditionsStr = map (damagedPrefix      ++) (sequence $ arrangements (SpringRow (drop damagedRun conditionsStr)             damagedRuns))
    | otherwise                                = map (head conditionsStr : ) (sequence $ arrangements (SpringRow (tail            conditionsStr) (damagedRun:damagedRuns)))
  where damagedPrefix = replicate damagedRun '#'

-- arrangementsWithPrefix
-- arrangementsWithPrefix prefix = 

day12part1 = do
  contents <- readFile "day12 (example).csv"
  mapM_ print . map arrangements . map readSpringRow . lines $ contents

-- day12part2 = do
  -- contents <- readFile "day12 (data).csv"
  -- print . map SpringRow . lines $ contents