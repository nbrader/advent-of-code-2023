#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22 --package split-0.2.3.5 --package parallel-3.2.2.0

---------------------------------------------
---------------------------------------------
----  Day 14:  Parabolic Reflector Dish  ----
---------------------------------------------
---------------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package linear-1.22 -- -threaded -O2 '.\day14.hs'
-}

------------
-- Output --
------------
-- *Main> day14part1
-- 

-- *Main> day14part2
-- 


-------------
-- Imports --
-------------
import Data.List (foldl', nub, sort, transpose, isPrefixOf, findIndex, findIndices, intercalate, reverse, partition, intersperse, scanl')
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
main = day14part2

readColumns :: String -> [String]
readColumns = transpose . lines

roll = map (intercalate "#" . map ((\(rocks,spaces) -> rocks ++ spaces) . partition (== 'O'))) . map (splitOn "#")
load = sum . map sum . map (zipWith (\i c -> i * if c == 'O' then 1 else 0) [1..]) . map reverse
rotateRocks = transpose . reverse

day14part1 = do
  contents <- readFile "day14 (data).csv"
  let cols = readColumns $ contents
  print . load . roll $ cols

runAllOn :: [a -> a] -> a -> a
runAllOn = flip (foldl' (flip ($!)))

runAllOnAndList :: [a -> a] -> a -> [a]
runAllOnAndList = flip (scanl' (flip ($!)))

day14part2 = do
  contents <- readFile "day14 (example).csv"
  let cols = readColumns $ contents
  mapM_ putStrLn cols
  putStrLn ""
  -- print . load . foldr (flip (.)) id (intersperse rotateRocks (concat $ replicate 1000000000 (replicate 4 roll))) $ cols
  -- mapM_ ((>> putStrLn "") . mapM_ putStrLn) $ runAllOnAndList (intersperse rotateRocks (concat $ replicate (8*202) (replicate 4 roll))) (rotateRocks . rotateRocks . rotateRocks $ cols)
  mapM_ print . map load $ runAllOnAndList (intersperse rotateRocks (concat $ replicate (8*202) (replicate 4 roll))) (rotateRocks . rotateRocks . rotateRocks $ cols)