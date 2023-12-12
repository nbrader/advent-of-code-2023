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
import Data.List (foldl', nub, sort, transpose, isPrefixOf, findIndex)
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

ignore _ x = x

arrangements :: SpringRow -> [String]
arrangements sr@(SpringRow ""            []                      ) = [""]
arrangements sr@(SpringRow ""            (damagedRun:damagedRuns)) = []
arrangements sr@(SpringRow conditionsStr [])
    | all (\c -> c == '.' || c == '?') conditionsStr = [allAsUndamaged]
    | otherwise                                      = []
  where allAsUndamaged = replicate (length conditionsStr) '.'
arrangements sr@(SpringRow conditionsStr (damagedRun:damagedRuns))
    |    enoughCharsToHaveDamagedRun && matchesDamagedRun
      && anyFollowingCharIsNonDamaged = (map ((damagedPrefix ++ followingCharStr) ++) . arrangements $ (SpringRow (drop (damagedRun + length followingCharStr) conditionsStr) damagedRuns)) ++ arrangementsStartingWithUndamaged
    | otherwise                       = arrangementsStartingWithUndamaged
  where damagedPrefix = replicate damagedRun '#'
        enoughCharsToHaveDamagedRun = length conditionsStr >= damagedRun
        matchesDamagedRun = all (\c -> c == '#' || c == '?') $ take damagedRun conditionsStr
        anyFollowingCharIsNonDamaged = (\xs -> case xs of {[] -> True; (x':_) -> x' /= '#'}) . drop damagedRun $ conditionsStr
        followingCharStr = map (const '.') . (\xs -> case xs of {[] -> ""; (x':_) -> [x']}) . drop damagedRun $ conditionsStr
        headUndamaged = (\c -> c == '.' || c == '?') . head $ conditionsStr
        arrangementsStartingWithUndamaged
            | headUndamaged = map ('.':) . arrangements $ (SpringRow (tail conditionsStr) (damagedRun:damagedRuns))
            | otherwise     = []

-- foldedArrangements :: Int -> SpringRow -> [String]
foldedArrangements dupeCount sr@(SpringRow conditionsStr damagedRuns)
    = case after of
        [] -> error "Handle this case if you see this message!"
        _ -> concat $ do
            numOfInitRuns <- [0 .. (length damagedRuns)]
            let (initRuns,finalRuns) = splitAt numOfInitRuns damagedRuns
            
            return $ map concat $ sequence $ concat [
                            [arrangements (SpringRow before initRuns)],
                            replicate (dupeCount-1) (arrangements (SpringRow (concat [after, '?':before]) (concat [finalRuns, initRuns]))),
                            [arrangements (SpringRow after finalRuns)]
                            ]
  where (before,after) = break (== '.') conditionsStr

day12part1 = do
  contents <- readFile "day12 (data).csv"
  print . sum . map length . map arrangements . map readSpringRow . lines $ contents

day12part2 = do
  contents <- readFile "day12 (data).csv"
  print . sum . map length . map (foldedArrangements 5) . map readSpringRow . lines $ contents