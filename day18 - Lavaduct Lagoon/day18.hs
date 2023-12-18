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
import Linear.V2
import Debug.Trace (trace)


-------------
-- Program --
-------------
main = day18part1

data DigLine = DigLine {digLineDir :: V2 Int, digLineLength :: Int, digLineCol :: String}

vUp = V2 ( 0) (-1)
vDn = V2 ( 0) ( 1)
vLt = V2 (-1) ( 0)
vRt = V2 ( 1) ( 0)

readDirStr :: String -> V2 Int
readDirStr "U" = vUp
readDirStr "D" = vDn
readDirStr "L" = vLt
readDirStr "R" = vRt

readDigLines :: String -> [DigLine]
readDigLines inStr = do
    row <- rows
    let [dirStr, lenStr, colStr] = words row
    let dir = readDirStr dirStr
    let len = read lenStr
    let col = colStr
    return $ DigLine dir len col
  where rows = lines inStr


data Graph = Graph {graphEdgesFromV2 :: M.Map (V2 Int) [V2 Int], graphStart :: V2 Int, graphBounds :: V2 Int} deriving (Show)

readGraph :: String -> Graph
readGraph inStr = Graph edges startPos bounds
  where bounds = undefined
        startPos = V2 0 0
        edges = undefined

lookupList :: (Ord k) => k -> M.Map k [a] -> [a]
lookupList k m = concat . maybeToList $ M.lookup k m

pathFromStart :: Graph -> [V2 Int]
pathFromStart (Graph edges startPos bounds)
    = until returned step [startPos]
  where returned (v:[]) = False
        returned (v:_ ) = v == startPos
        
        step (v:visited) = newV:v:visited
          where newVs = map (v+) $ lookupList v edges
                newV = if null visited
                        then head newVs
                        else let prevV = head visited
                             in head $ filter (/= prevV) newVs

day18part1 = do
  contents <- readFile "day18 (data 3).csv"
  let graph = readGraph contents
  print . (`div` 2) . length . pathFromStart $ graph

rot90 :: V2 Int -> V2 Int
rot90 (V2 x y) = V2 (-y) x

getX (V2 x y) = x
getY (V2 x y) = y

inBounds :: V2 Int -> V2 Int -> Bool
inBounds (V2 x y) (V2 width height) = x >= 0 && x < width && y >= 0 && y < height

findInterior :: V2 Int -> [V2 Int] -> [V2 Int]
findInterior bounds positions = nub . concat . map (\((pos,left),(prevPos, prevLt)) -> rayUntilPath (pos+left) left ++ rayUntilPath (prevPos+left) left) $ zip posAndSideDirs prevs
  where leftInterior = all (\(pos,left) -> rayHitBounds (pos+left) left) posAndLeftDirs
        
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
