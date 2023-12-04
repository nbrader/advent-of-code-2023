#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22

--------------------------------
--------------------------------
----  Day 4:  Scratchcards  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22
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
import Data.Char (isDigit)
import Data.List (sort, tails, isPrefixOf, groupBy, find, nub)
import Data.List.Split (splitOn)
import Data.Maybe
import Debug.Trace (trace)
import Data.Map as M hiding (map, filter, take, drop)
import Linear hiding (trace)
import Linear.V2
import Data.Tuple (swap)
import Data.Function (on)


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
  contents <- readFile "day4 (data 3).csv"
  let total = sum . map (pointsFromGame . readGame) . lines $ contents
  print $ total

cards :: [Game] -> Int
cards [] = 0
cards (g:gs) = case matches g of
            0 -> 1
            n -> 1 + (sum $ map cards (take n $ tails gs))

memoized_cards :: [Game] -> Int -> Int
memoized_cards gs = go
   where go :: Int -> Int
         go = (map cards [0 ..] !!)
           where cards i = case matches (gs !! i) of
                            0 -> 1
                            n -> 1 + (sum $ map go [(i+1) .. (min (length gs - 1) (i+n))])

day4part2 = do
  contents <- readFile "day4 (data 3).csv"
  let total = sum . (\gs -> map (memoized_cards gs) ([0..(length gs - 1)])) . map readGame . lines $ contents
  print $ total
