#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package fastmemo-0.1.1

--------------------------------
--------------------------------
----  Day 4:  Scratchcards  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc -- '.\day4.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day4part1
-- 23235

-- *Main> day4part2
-- 5920640


-------------
-- Imports --
-------------
import Data.List (sort)
import Data.Function (fix)
import Data.Function.FastMemo (memoize)


-------------
-- Program --
-------------
main = day4part1

data Game = Game {gameID :: Int, gameWinningNumbers :: [Int], gameMyNumbers :: [Int]} deriving (Show)

readGame :: String -> Game
readGame inStr = Game { gameID = read idStr,
                        gameWinningNumbers = map read . words $ winNumsStr,
                        gameMyNumbers = map read . words $ myNumsStr}
  where (idStr, after1) = break (==':') (drop (length "Game ") $ inStr)
        (winNumsStr, after2) = break (=='|') (drop (length ": ") $ after1)
        myNumsStr = (drop (length "| ") $ after2)

pointsFromGame :: Game -> Int
pointsFromGame = pointsFromMatches . matches

matches :: Game -> Int
matches g = length (sort (gameWinningNumbers g) `intersect` sort (gameMyNumbers g))

pointsFromMatches 0 = 0
pointsFromMatches n = 2^(n-1)

intersect :: [Int] -> [Int] -> [Int]
intersect [] = const []
intersect xs = filter (`elem` xs)

day4part1 = do
  contents <- readFile "day4 (data).csv"
  let total = sum . map (pointsFromGame . readGame) . lines $ contents
  print $ total

-- faster but less generic
-- memoize :: (Int -> a) -> (Int -> a)
-- memoize f = (map f [0 ..] !!)

cards :: [Game] -> (Int -> Int) -> (Int -> Int)
cards gs = go
   where go :: (Int -> Int) -> (Int -> Int)
         go f i = case matches (gs !! i) of
                    0 -> 1
                    n -> 1 + (sum $ map f [(i+1) .. (min (length gs - 1) (i+n))])

day4part2 = do
  contents <- readFile "day4 (data).csv"
  let total = sum . (\gs -> map (fix (memoize . cards gs)) ([0..(length gs - 1)])) . map readGame . lines $ contents
  print $ total
