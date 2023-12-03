#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5 --package containers-0.6.5.1

----------------------------------
----------------------------------
----  Day 2:  Cube Conundrum  ----
----------------------------------
----------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 --package containers-0.6.5.1 -- '.\day2.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day2part1
-- 2447

-- *Main> day2part2
-- 56322


-------------
-- Imports --
-------------
import Data.Char (isDigit)
import Data.List (sort, tails, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe
import Debug.Trace (trace)


-------------
-- Program --
-------------
main = day2part2

data CubeSet = CubeSet {cubeSetRed :: Int, cubeSetGreen :: Int, cubeSetBlue :: Int} deriving (Show)
data Game = Game {gameID :: Int, gameCubeSets :: [CubeSet] } deriving (Show)

readCubeSet :: String -> CubeSet
readCubeSet inStr = CubeSet { cubeSetRed   = fromMaybe 0 . listToMaybe . map fst . filter (\(n,col) -> col == "red") $ amounts,
                              cubeSetGreen = fromMaybe 0 . listToMaybe . map fst . filter (\(n,col) -> col == "green") $ amounts,
                              cubeSetBlue  = fromMaybe 0 . listToMaybe . map fst . filter (\(n,col) -> col == "blue") $ amounts}
  where amounts = map (\[n,col] -> (read n, col)) . map (\xs -> splitOn " " xs) . splitOn ", " $ inStr

readGame :: String -> Game
readGame inStr = Game { gameID       = read idStr,
                        gameCubeSets = map readCubeSet (splitOn "; " cubeSetStr) }
  where (idStr, after1) = break (==':') (drop (length "Game ") $ inStr)
        cubeSetStr = (drop (length ": ") $ after1)

day2part1 = do
  contents <- readFile "day2 (data).csv"
  let total = sum . map gameID . filter (\game -> all (\cubeSet -> cubeSetRed cubeSet <= 12 && cubeSetGreen cubeSet <= 13 && cubeSetBlue cubeSet <= 14) (gameCubeSets game)) . map readGame . lines $ contents
  print $ total

power :: CubeSet -> Int
power cubeSet = cubeSetRed cubeSet * cubeSetGreen cubeSet * cubeSetBlue cubeSet

minCubeSet :: Game -> CubeSet
minCubeSet game = (CubeSet (maximum . map cubeSetRed $ gameCubeSets game) (maximum . map cubeSetGreen $ gameCubeSets game) (maximum . map cubeSetBlue $ gameCubeSets game))

day2part2 = do
  contents <- readFile "day2 (data).csv"
  let total = sum . map power . map minCubeSet . map readGame . lines $ contents
  print $ total