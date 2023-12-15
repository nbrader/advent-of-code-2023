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
import qualified Data.IntSet as S


-------------
-- Program --
-------------
main = day14part2

readColumns :: String -> [String]
readColumns = lines

roll = map (intercalate "#" . map ((\(rocks,spaces) -> rocks ++ spaces) . partition (== 'O'))) . map (splitOn "#")
load = sum . map sum . map (zipWith (\i c -> i * if c == 'O' then 1 else 0) [1..])
rotateCW90     = transpose . reverse
rotateAntiCW90 = reverse . transpose
rotate180      = map reverse . reverse

day14part1 = do
  contents <- readFile "day14 (data).csv"
  let cols = readColumns $ contents
  print . load . rotate180 . roll . rotateAntiCW90 $ cols

runAllOn :: [a -> a] -> a -> a
runAllOn = flip (foldl' (flip ($!)))

runAllOnAndList :: [a -> a] -> a -> [a]
runAllOnAndList = flip (scanl' (flip ($!)))

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

getCycle :: [Int] -> (Int,Int)
getCycle = go mempty mempty Nothing
  where go :: S.IntSet -> [Int] -> [Int] -> Maybe [Int] -> (Int,Int)
        go visitedSet visitedList Nothing (x:xs)
            | x `member` visitedSet = go takeWhileInclusive (/= x) visitedList
            | otherwise = 
        go visitedSet visitedList maybeUnverifiedCycleElems (x:xs)
            | x `member` visitedSet = go takeWhileInclusive (/= x) visitedList
            | otherwise = 

-- I should make this detect when the load value repeats and then calculate where it would land at a billionth spin cycle
day14part2 = do
  contents <- readFile "day14 (example).csv"
  let cols = readColumns $ contents
  mapM_ print . map load {-. map head . chunksOf 8 . drop 5-} $ runAllOnAndList (intersperse rotateCW90 (concat $ replicate 1 (replicate 4 roll))) (rotateAntiCW90 $ cols)