#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22

-------------------------------
-------------------------------
----  Day 3:  Gear Ratios  ----
-------------------------------
-------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22
-}

------------
-- Output --
------------
-- *Main> day3part1
-- 514969

-- *Main> day3part2
-- 78915902


-------------
-- Imports --
-------------
import Data.Char (isDigit)
import Data.List (sort, tails, isPrefixOf, groupBy, find, nub)
import Data.List.Split (splitOn)
import Data.Maybe
import Debug.Trace (trace)
import Data.Map as M hiding (map, filter, take)
import Linear
import Linear.V2
import Data.Tuple (swap)
import Data.Function (on)


-------------
-- Program --
-------------
main = day3part1

data Number = Number {numberInt :: Int, numV2s :: [V2 Int]} deriving (Show, Ord, Eq)
data Symbol = Symbol {symbolChar :: Char, symbolV2 :: V2 Int} deriving (Show, Ord, Eq)

data Schematic = Schematic {schematicDims :: V2 Int, schematicNums :: [Number], schematicNumFromPos :: M.Map (V2 Int) Number, schematicSymbols :: [Symbol], schematicSymbolToNums :: M.Map Symbol [Number]}

readSchematic :: String -> Schematic
readSchematic inStr = Schematic dims nums numFromPos symbols symbolToNums
  where rows = lines inStr
        
        dims = V2 (length . head $ rows) (length rows)
        
        symbolsAndDigitsRows :: [[Symbol]]
        symbolsAndDigitsRows = [[Symbol c (V2 i j) | (i,c) <- zip [0..] row] | (j,row) <- zip [0..] rows]
        
        nums :: [Number]
        nums = concat $ ((map (map (\symbolList -> let n = read (map symbolChar $ symbolList) :: Int in Number n (map symbolV2 symbolList)))) :: [[[Symbol]]] -> [[Number]]) . ((map (filter (isDigit . symbolChar . head))) :: [[[Symbol]]] -> [[[Symbol]]]) . (map ((groupBy (((==) `on` (isDigit . symbolChar)))) :: [Symbol] -> [[Symbol]]) :: [[Symbol]] -> [[[Symbol]]]) $ symbolsAndDigitsRows
        
        numFromPos = M.fromList [(pos,num) | num <- nums, pos <- numV2s num]
        
        symbols = filter ((\c -> not (isDigit c) && not (c == '.')) . symbolChar) . concat $ symbolsAndDigitsRows
        
        dirs = [V2 x y | let componentOptions = [-1, 0, 1], x <- componentOptions, y <- componentOptions, not (x == 0 && y == 0)]
        
        symbolNumberPairs = [let positions = map ((symbolV2 s) +) dirs in (s, nub $ concatMap (\pos -> case M.lookup pos numFromPos of {Nothing -> []; Just num -> [num]}) positions) | s <- symbols]
        symbolToNums = M.fromList symbolNumberPairs

day3part1 = do
  contents <- readFile "day3 (data).csv"
  let total = sum . concat . map snd . (\schematic -> map (fmap (map numberInt) . (\symbol -> (symbol, fromJust $ M.lookup symbol (schematicSymbolToNums schematic)))) $ schematicSymbols schematic) . readSchematic $ contents
  print $ total

day3part2 = do
  contents <- readFile "day3 (data).csv"
  let total = sum . map (\(s,ns) -> product ns) . filter ((== 2) . length . snd) . filter ((== '*') . symbolChar . fst) . (\schematic -> map (fmap (map numberInt) . (\symbol -> (symbol, fromJust $ M.lookup symbol (schematicSymbolToNums schematic)))) $ schematicSymbols schematic) . readSchematic $ contents
  print $ total
