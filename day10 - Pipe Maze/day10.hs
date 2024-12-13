#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22

------------------------------
------------------------------
----  Day 10:  Pipe Maze  ----
------------------------------
------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package linear-1.22 -- '.\day10.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day10part1
-- 6725

-- *Main> day10part2
-- 383


-------------
-- Imports --
-------------
import Data.List (foldl', nub)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Linear.V2


-------------
-- Program --
-------------
main = day10part1

data Graph = Graph {graphEdgesFromV2 :: M.Map (V2 Int) [V2 Int], graphStart :: V2 Int, graphBounds :: V2 Int} deriving (Show)

vUp    = V2 ( 0) (-1)
vDown  = V2 ( 0) ( 1)
vLeft  = V2 (-1) ( 0)
vRight = V2 ( 1) ( 0)

dispsFromChar :: Char -> [V2 Int]
dispsFromChar '|' = [vUp, vDown]
dispsFromChar '-' = [vLeft, vRight]
dispsFromChar 'L' = [vUp, vRight]
dispsFromChar 'J' = [vUp, vLeft]
dispsFromChar '7' = [vDown, vLeft]
dispsFromChar 'F' = [vDown, vRight]
dispsFromChar '.' = []
dispsFromChar c   = error $ "UnknownChar: " ++ [c]

charFromConnectedDirs :: Bool -> Bool -> Bool -> Bool -> Char
charFromConnectedDirs (( True)) ( True) (False) (False) = '|'
charFromConnectedDirs ((False)) (False) ( True) ( True) = '-' 
charFromConnectedDirs (( True)) (False) (False) ( True) = 'L'
charFromConnectedDirs (( True)) (False) ( True) (False) = 'J'
charFromConnectedDirs ((False)) ( True) ( True) (False) = '7'
charFromConnectedDirs ((False)) ( True) (False) ( True) = 'F'
charFromConnectedDirs ((False)) (False) (False) (False) = '.'
-- charFromConnectedDirs connUp connDown connLeft connRight

readBounds :: String -> V2 Int
readBounds inStr = V2 width height
  where rows = lines inStr
        width  = length . head . lines $ inStr
        height = length        . lines $ inStr

readStart :: String -> V2 Int -> (V2 Int, Char)
readStart inStr (V2 width height) = (V2 sX sY, char)
  where rows = lines inStr
        
        (beforeS,s:afterS) = break (== 'S') inStr
        sX = subtract 1 . length . last . lines $ beforeS++[s]
        sY = subtract 1 . length . lines $ beforeS++[s]
        
        maybeCharUp    = if sY > 0        then Just ((rows !! (sY-1)) !!  sX   ) else Nothing
        maybeCharDown  = if sY < height-1 then Just ((rows !! (sY+1)) !!  sX   ) else Nothing
        maybeCharLeft  = if sX > 0        then Just ((rows !!  sY)    !! (sX-1)) else Nothing
        maybeCharRight = if sX < width-1  then Just ((rows !!  sY)    !! (sX+1)) else Nothing
        
        connUp    = case maybeCharUp    of {Just c -> vDown  `elem` dispsFromChar c; Nothing -> False}
        connDown  = case maybeCharDown  of {Just c -> vUp    `elem` dispsFromChar c; Nothing -> False}
        connLeft  = case maybeCharLeft  of {Just c -> vRight `elem` dispsFromChar c; Nothing -> False}
        connRight = case maybeCharRight of {Just c -> vLeft  `elem` dispsFromChar c; Nothing -> False}
        
        char = charFromConnectedDirs connUp connDown connLeft connRight

readChar2Ds :: String -> [(V2 Int, Char)]
readChar2Ds inStr = do
    let rows = lines inStr
    (y,row)  <- zip [0..] rows
    (x,char) <- zip [0..] row
    
    return (V2 x y, char)
    
readPosToLabelMap :: String -> M.Map (V2 Int) Char
readPosToLabelMap = M.fromList . readChar2Ds

readGraph :: String -> Graph
readGraph inStr = Graph edges startPos bounds
  where posToLabels = readPosToLabelMap inStr
        bounds = readBounds inStr
        (startPos, startChar) = readStart inStr bounds
        posToLabelsReplacedStart = M.insert startPos startChar posToLabels
        
        edges = M.map dispsFromChar posToLabelsReplacedStart

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

day10part1 = do
  contents <- readFile "day10 (data 3).csv"
  let graph = readGraph contents
  print . (`div` 2) . length . pathFromStart $ graph

rot90 :: V2 Int -> V2 Int
rot90 (V2 x y) = V2 (-y) x

getX (V2 x y) = x
getY (V2 x y) = y

inBounds :: V2 Int -> V2 Int -> Bool
inBounds (V2 x y) (V2 width height) = x >= 0 && x < width && y >= 0 && y < height

findInterior :: V2 Int -> [V2 Int] -> [V2 Int]
findInterior bounds positions = nub . concat . map (\((pos,left),(prevPos, prevLeft)) -> rayUntilPath (pos+left) left ++ rayUntilPath (prevPos+left) left) $ zip posAndSideDirs prevs
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

day10part2 = do
  contents <- readFile "day10 (data 2).csv"
  let graph = readGraph contents
      bounds = graphBounds graph
      path = pathFromStart graph
      interior = findInterior bounds path
  -- putStrLn . writeCharsOnInput 'I' interior . writeCharsOnInput '*' path $ contents
  print $ length interior
