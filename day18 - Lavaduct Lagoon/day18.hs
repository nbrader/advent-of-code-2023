#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22

-----------------------------------
-----------------------------------
----  Day 18: Lavaduct Lagoon  ----
-----------------------------------
-----------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package linear-1.22 -- '.\day18.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day18part1
-- 

-- *Main> day18part2
-- 


-------------
-- Imports --
-------------
import Data.List (foldl', nub, sort)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybeToList)
import Linear hiding (trace)
import Linear.V2
import Debug.Trace (trace)


-------------
-- Program --
-------------
main = day18part2

data DigLine = DigLine {digLineDir :: V2 Int, digLineLength :: Int} deriving (Show)

vUp = V2 ( 0) (-1)
vDn = V2 ( 0) ( 1)
vLt = V2 (-1) ( 0)
vRt = V2 ( 1) ( 0)

readDirStr :: String -> V2 Int
readDirStr "U" = vUp
readDirStr "D" = vDn
readDirStr "L" = vLt
readDirStr "R" = vRt

readDirDigit :: Char -> V2 Int
readDirDigit '3' = vUp
readDirDigit '1' = vDn
readDirDigit '2' = vLt
readDirDigit '0' = vRt
readDirDigit c = error (show c)

charToHexDigit :: Char -> Int
charToHexDigit 'a' = 10
charToHexDigit 'b' = 11
charToHexDigit 'c' = 12
charToHexDigit 'd' = 13
charToHexDigit 'e' = 14
charToHexDigit 'f' = 15
charToHexDigit digit = read [digit]

readHex :: String -> Int
readHex = sum . zipWith (\x y -> y^x) [0..] . map charToHexDigit

readDigLines :: String -> [DigLine]
readDigLines inStr = do
    row <- rows
    let [dirStr, lenStr, hexStr] = words row
    let dir = readDirStr dirStr
    let len = read lenStr
    return $ DigLine dir len
  where rows = lines inStr

readDigLines2 :: String -> [DigLine]
readDigLines2 inStr = do
    row <- rows
    let [dirStr, lenStr, hexStr] = words row
    let len = readHex . tail . tail . init $ hexStr
    let dir = readDirDigit . last . init $ hexStr
    return $ DigLine dir len
  where rows = lines inStr

start = V2 0 0

pathFromStart :: [DigLine] -> [V2 Int]
pathFromStart = foldl' step [start]
  where step (v:visited) (DigLine dir len) = [v + i*^dir | i <- [len, (len-1) .. 1]] ++ v:visited

day18part1 = do
  contents <- readFile "day18 (data).csv"
  let edgesPositions = drop 1 . pathFromStart . readDigLines $ contents
  let interior = findInterior $ edgesPositions
  print (length edgesPositions + length interior)

day18part2 = do
  contents <- readFile "day18 (example).csv"
  let edgesPositions = drop 1 . pathFromStart . readDigLines2 $ contents
  let interior = findInterior $ edgesPositions
  print (length edgesPositions + length interior)

rot90 :: V2 Int -> V2 Int
rot90 (V2 x y) = V2 (-y) x

getX (V2 x y) = x
getY (V2 x y) = y

inBounds :: V2 Int -> (V2 Int, V2 Int) -> Bool
inBounds (V2 x y) (V2 minX minY, V2 maxX maxY) = x >= minX && x < maxX && y >= minY && y < maxY

findInterior :: [V2 Int] -> [V2 Int]
findInterior positions = nub . concat . map (\((pos,left),(prevPos, prevLt)) -> rayUntilPath (pos+left) left ++ rayUntilPath (prevPos+left) left) $ zip posAndSideDirs prevs
  where allXs = map (\(V2 x y) -> x) positions
        allYs = map (\(V2 x y) -> y) positions
        bounds = (V2 (minimum allXs) (minimum allYs), V2 (maximum allXs) (maximum allYs))
        
        leftInterior = all (\(pos,left) -> rayHitBounds (pos+left) left) posAndLeftDirs
        
        flipIfRightInterior
            | leftInterior = id
            | otherwise    = negate
        
        diffs :: [V2 Int]
        diffs = zipWith subtract (init positions) (tail positions)
        
        posAndLeftDirs :: [(V2 Int, V2 Int)]
        posAndLeftDirs = zip positions (map rot90 diffs)
        
        posAndSideDirs :: [(V2 Int, V2 Int)]
        posAndSideDirs = zip positions (map (flipIfRightInterior . rot90) diffs)
        
        prevs = drop 1 $ cycle posAndSideDirs
        
        rayUntilPath :: V2 Int -> V2 Int -> [V2 Int]
        rayUntilPath start dir = tail $ until ((`elem` positions) . head) (\(v:vs) -> (v+dir):v:vs) [start]
        
        rayHitBounds :: V2 Int -> V2 Int -> Bool
        rayHitBounds start dir = not . all (`inBounds` bounds) $ until ((\pos -> pos `elem` positions || not (pos `inBounds` bounds)) . head) (\(v:vs) -> (v+dir):v:vs) [start]

writeCharsOnInput :: Char -> [V2 Int] -> String -> String
writeCharsOnInput newChar positions inStr = unlines $ foldl' (writeChar newChar) (lines inStr) positions

writeChar :: Char -> [String] -> V2 Int -> [String]
writeChar newChar rows (V2 x y)= do
    (y',row)  <- zip [0..] rows
    
    return $ if y' /= y
              then row
              else do
                (x',oldChar) <- zip [0..] row
                
                return $ if x /= x'
                          then oldChar
                          else newChar
