#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5

---------------------------------------------
---------------------------------------------
----  Day 14:  Parabolic Reflector Dish  ----
---------------------------------------------
---------------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 -- -threaded -O2 '.\day14.hs'
-}

------------
-- Output --
------------
-- *Main> day14part1
-- 109466

-- *Main> day14part2
-- 94585


-------------
-- Imports --
-------------
import Data.List (foldl', nub, transpose, isPrefixOf, intercalate, reverse, partition, intersperse, scanl')
import Data.List.Split (splitOn)
import qualified Data.IntSet as S


-------------
-- Program --
-------------
main = day14part2

readColumns :: String -> [String]
readColumns = lines

roll = map (intercalate "#" . map ((\(rocks,spaces) -> rocks ++ spaces) . partition (== 'O'))) . map (splitOn "#")
load = sum . map sum . map (zipWith (\i c -> i * if c == 'O' then 1 else 0) [1..]) . rotate180
rotateCW90     = transpose . reverse
rotateAntiCW90 = reverse . transpose
rotate180      = map reverse . reverse

day14part1 = do
  contents <- readFile "day14 (data).csv"
  let cols = readColumns $ contents
  print . load . roll $ rotateAntiCW90 cols

runAllOn' :: [a -> a] -> a -> a
runAllOn' = flip (foldl' (flip ($!)))

runAllOnAndList' :: [a -> a] -> a -> [a]
runAllOnAndList' = flip (scanl' (flip ($!)))

runAllOnAndList :: [a -> a] -> a -> [a]
runAllOnAndList = flip (scanl (flip ($)))

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

getCycle :: [Int] -> ([Int], Int)
getCycle = go mempty mempty 0
  where go :: S.IntSet -> [Int] -> Int -> [Int] -> ([Int], Int)
        go visitedSet visitedList cycleStart (x:xs) 
            | x `S.member` visitedSet && possibleCycle `isPrefixOf` (x:xs) && length possibleCycle `mod` 8 == 0 = (possibleCycle, cycleStart - length possibleCycle)
            | otherwise = go (S.insert x visitedSet) (x:visitedList) (cycleStart+1) xs
          where possibleCycle = reverse $ takeWhileInclusive (/= x) visitedList

getValueAssumingCycle :: Int -> ([Int], Int) -> Int
getValueAssumingCycle i (cycleList, cycleStart) = cycleList !! indexIntoCycleList
  where cycleLength = length cycleList
        offset = (-cycleStart) `mod` cycleLength
        indexIntoCycleList = (i + offset) `mod` cycleLength

-- I should make this detect when the load value repeats and then calculate where it would land at a billionth spin cycle
day14part2 = do
  contents <- readFile "day14 (data).csv"
  let cols = readColumns $ contents
  print . getValueAssumingCycle 1000000000 . getCycle . map load $ runAllOnAndList (intersperse rotateCW90 (repeat roll)) (rotateAntiCW90 cols)