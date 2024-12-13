#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22

-------------------------------
-------------------------------
----  Day 3:  Gear Ratios  ----
-------------------------------
-------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package linear-1.22 -- '.\day3.hs' -O2
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
import Data.List (groupBy, nub)
import Data.Maybe (maybeToList)
import Data.Map as M hiding (map, filter, take)
import Linear.V2
import Data.Function (on)
import Control.Monad (guard)


-------------
-- Program --
-------------
main = day3part1

data Number = Number {
    numberInt :: Int,
    numV2s :: [V2 Int] -- positions of digits of number
    } deriving (Show, Ord, Eq)

data Char2D = Char2D {
    symbolChar :: Char,
    symbolV2 :: V2 Int
    } deriving (Show, Ord, Eq)

data Schematic = Schematic {
    schematicSymbolToNums :: [(Char2D,[Int])]
    }

readSchematic :: String -> Schematic
readSchematic inStr = Schematic symbolToNums
  where rows = lines inStr
        
        symbolsAndDigitsRows :: [[Char2D]]
        symbolsAndDigitsRows = do
            (j,row) <- zip [0..] rows
            return $ do
                (i,c) <- zip [0..] row
                return $ Char2D c (V2 i j)
        
        digitsToNumber :: [Char2D] -> Number
        digitsToNumber symbolList
            = let n :: Int
                  n = read . map symbolChar $ symbolList
              in Number n (map symbolV2 symbolList)
        
        removeNonDigitGroups :: [[Char2D]] -> [[Char2D]]
        removeNonDigitGroups = (filter (isDigit . symbolChar . head))
        
        groupByIsDigit :: [Char2D] -> [[Char2D]]
        groupByIsDigit = groupBy (((==) `on` (isDigit . symbolChar)))
        
        nums :: [Number]
        nums = concat . map (map digitsToNumber)
                      . map removeNonDigitGroups
                      . map groupByIsDigit $ symbolsAndDigitsRows
        
        numFromPos = M.fromList [(pos, numberInt num) | num <- nums, pos <- numV2s num]
        
        nonDigit symbol = let char = symbolChar symbol
                          in not (isDigit char) && not (char == '.')
        
        symbols = filter nonDigit . concat $ symbolsAndDigitsRows
        
        dirs = do
            let componentOptions = [-1, 0, 1]
            x <- componentOptions
            y <- componentOptions
            guard $ not (x == 0 && y == 0)
            return $ V2 x y
        
        symbolToNums :: [(Char2D,[Int])]
        symbolToNums = do
            s <- symbols
            let positions = map ((symbolV2 s) +) dirs
                numbers = nub . concatMap (\pos -> maybeToList $ M.lookup pos numFromPos) $ positions
            return (s, numbers)

day3part1 = do
  contents <- readFile "day3 (data).csv"
  let schematic = readSchematic contents
      symbolToNums = schematicSymbolToNums schematic
      
      total = sum . concat
                  . map snd
                  $ symbolToNums
  print $ total

day3part2 = do
  contents <- readFile "day3 (data).csv"
  let schematic = readSchematic contents
      symbolToNums = schematicSymbolToNums schematic

  
  let total = sum . map (\(s,ns) -> product ns)
                  . filter ((== 2) . length . snd)
                  . filter ((== '*') . symbolChar . fst)
                  $ symbolToNums
  print $ total