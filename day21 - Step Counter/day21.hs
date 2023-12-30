#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22 --package split-0.2.3.5 --package deque-0.4.4.1

--------------------------------
--------------------------------
----  Day 21: Step Counter  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package linear-1.22 --package deque-0.4.4.1 -- '.\day21.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day21part1
-- 

-- *Main> day21part2
-- 


-------------
-- Imports --
-------------
import Data.List (foldl', nub, sort, sortBy, groupBy, delete, find)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybeToList, catMaybes)
import Linear hiding (trace)
import Linear.V2
import Debug.Trace (trace)
import Data.Ord
import Data.Function
import Data.Tuple
import GHC.Exts as F
import Data.Bits
import Control.Monad (guard)
import Data.Monoid
import Data.Foldable
import Data.Order


-------------
-- Program --
-------------
main = day21part1


-- Each obj has a shape of Points encoded as Integer.
type SinglePoint = Integer
type Points = Integer

data World = World {worldBG :: Char, worldLayers :: M.Map Char Points, worldPoints :: M.Map Char SinglePoint, worldWidth :: Int}

hasPoint :: Char -> (Int,Int) -> World -> Bool
hasPoint = undefined

readWorld :: String -> World
readWorld = undefined

readChar2Ds :: [Char] -> [((Int,Int), Char)]
readChar2Ds inStr = do
    let rows = lines inStr
    (y,row)  <- zip [0..] rows
    (x,char) <- zip [0..] row
    
    return ((x, y), char)

showWorld :: Int -> (Char -> Char -> Ordering) -> World -> String
showWorld height charZOrder world = undefined

-- Assumes worlds are same size
-- Keeps characters from the first where they coincide
combineWorlds :: World -> World -> World
combineWorlds w1 w2
    = w1 { worldLayers = unionWithKey combineLayers (worldLayers w1) (worldLayers w2),
           worldPoints = unionWithKey combinePoints (worldPoints w1) (worldPoints w2) }
  where combineLayers :: Char -> Points -> Points
        combineLayers = undefined
        
        combinePoints :: Char -> SinglePoint -> SinglePoint
        combinePoints = undefined

data Obj = Obj {
    objPoints  :: !Points,
    objRect :: !Rect } deriving (Show, Eq, Ord, Generic)

data Rect = Rect {
    rectLeft   :: !Int,
    rectBottom :: !Int,
    rectWidth  :: !Int,
    rectHeight :: !Int } deriving (Show, Eq, Ord, Generic)

rectRight r = rectLeft   r + rectWidth r
rectTop   r = rectBottom r + rectHeight r

type WorldStart = (Points, Obj, JetMove)

data World = World {
    worldNonCulledPoints   :: !Points,
    worldNextObjs         :: [Obj],
    worldFallingObj       :: !Obj,
    worldCullingHeight    :: !Int,
    worldHeightAboveCull  :: !Int,
    worldObjsStopped      :: !Int,
    worldPrevWorldsObjsStoppedAndCullingHeight :: Map WorldStart (Int,Int)} deriving (Show)

-- Every 8 bits of the integer represents a horizontal cross-section of the obj in the world as follows:
-- 
--       -- Least significant bit
--      |
--      V 
--      00111000 00001000 00001000
--
--  represents
--  
--      Wall    Wall   <-- Every bit position that's a multiple of 8 is where the walls of the world are (the right wall is just the left wall on the previous line)
--      |       |           The first bit (from least significant) is the wall and the next 7 bits are the mkPoints occupied by the falling obj within that row of possible places
--      |       |
--      V       V
--      |       | <-- The remaining rows are all zeroes as impled by the Integer type
--      |   #   | <-- The next  row up (from left) is the third  8 bits
--      |   #   | <-- The next  row up (from left) is the second 8 bits
--      | ###   | <-- The first row up (from left) is the first  8 bits


-- isOverlapping n n
-- time:  O(n) (approx.) [0 < n < 10000000000, 0 < secs < 7.97]
-- space: O(n) (approx.) [0 < n < 10000000000, 0 < bytes < 30000141704]
-- https://nux-pc/svn/Nux-SVN/My Programming/Scratchings/Advent of Code 2022/day17 (bits).hs/?r=1569
isOverlapping :: Points -> Points -> Bool
isOverlapping ps1 ps2 = (ps1 .&. ps2) /= zeroBits

isOverlappingWorld :: Obj -> World -> Bool
isOverlappingWorld r w = objRect r `isOverlappingHeight` (worldHeightAboveCull w) && objPoints r `isOverlapping` worldNonCulledPoints w

isOverlappingHeight :: Rect -> Int -> Bool
isOverlappingHeight (Rect _ b _ _) h = b <= h

union :: Points -> Points -> Points
union ps1 ps2 = ps1 .|. ps2

intersection :: Points -> Points -> Points
intersection ps1 ps2 = ps1 .&. ps2

inWall :: Obj -> Bool
inWall (Obj _ r) = rectLeft r < 1 || rectRight r > 7

onFloor :: Obj -> Bool
onFloor (Obj _ (Rect l b w h)) = b == 0

floorPoints = foldl' (.|.) zeroBits $ map mkPoint [(x,0) | x <- [0..7]]

move :: (Int,Int) -> Points -> Points
move (x,y) pts = pts `shiftL` (x + 8*y)

moveRect :: (Int,Int) -> Rect -> Rect
moveRect (x,y) (Rect l b w h) = Rect (l+x) (b+y) w h

moveObj :: (Int,Int) -> Obj -> Obj
moveObj (x,y) obj = Obj (move (x,y) (objPoints obj)) (moveRect (x,y) (objRect obj))

-- mkPoint (0,n)
-- time:  O(n) [0 < n < 50000000000, 0 < secs < 12.36]
-- space: O(n) [0 < n < 50000000000, 0 < bytes < 50000139672]
-- https://nux-pc/svn/Nux-SVN/My Programming/Scratchings/Advent of Code 2022/day17 (bits).hs/?r=1569
mkPoint :: (Int,Int) -> Points
mkPoint (x,y) = move (x,y) 1

-- startObj: positions 2 away from left wall and 3 up from top of world in nonculled world space. Note that (8*x,y) corresponds to within the wall which includes (0,0).
startObj :: Int -> Obj -> Obj
startObj worldHeightAboveCull (Obj pts rect)
    = let rect' = rect {
                    rectLeft   = 3,
                    rectBottom = worldHeightAboveCull + 3 }
      in  Obj {
            objPoints = (move (3, worldHeightAboveCull + 3)) pts,
            objRect   = rect' }

obj1, obj2, obj3, obj4, obj5 :: Obj
obj1 = (\pts -> Obj pts (Rect 0 0 3 0)) $ foldl' (.|.) zeroBits $ map mkPoint [(0,0), (1,0), (2,0), (3,0)]
obj2 = (\pts -> Obj pts (Rect 0 0 2 2)) $ foldl' (.|.) zeroBits $ map mkPoint [(1,0), (0,1), (1,1), (2,1), (1,2)]
obj3 = (\pts -> Obj pts (Rect 0 0 2 2)) $ foldl' (.|.) zeroBits $ map mkPoint [(0,0), (1,0), (2,0), (2,1), (2,2)]
obj4 = (\pts -> Obj pts (Rect 0 0 0 3)) $ foldl' (.|.) zeroBits $ map mkPoint [(0,0), (0,1), (0,2), (0,3)]
obj5 = (\pts -> Obj pts (Rect 0 0 1 1)) $ foldl' (.|.) zeroBits $ map mkPoint [(0,0), (1,0), (0,1), (1,1)]

initObjs = cycle [obj1, obj2, obj3, obj4, obj5]

showObjs :: Int -> Points -> Points -> String
showObjs rows worldNonCulledPoints fallingObj = intercalate "\n" . reverse $ [[if x `mod` 8 == 0 then '|' else (if pt `isOverlapping` worldNonCulledPoints then '1' else (if pt `isOverlapping` fallingObj then '2' else '0'))| x <-[0..8], let pt = mkPoint (x,y)] | y <-[0..(rows-1)]]






-- Positions
-- Knows how Positions is implemented START
newtype Positions = Positions {fromPositions :: Int} deriving (Show, Eq, Ord)
toPositions = Positions
-- Knows how Positions is implemented END

unionPositions :: [Positions] -> Positions
unionPositions = toPositions . sum . map fromPositions


-- Vec
-- Knows how Vec is implemented START
newtype Vec = Vec {fromVec :: (Int,Int)} deriving (Show, Eq, Ord)
toVec = Vec
-- Knows how Vec is implemented END

testPositionsVec :: Dims -> Vec -> Int -> Bool
testPositionsVec dims = undefined -- testBit

vecToSinglePositions :: Dims -> Vec -> Positions
vecToSinglePositions dims vec = toPositions $ bit x .<<. y
  where (x,y) = fromVec vec
        (width, height) = fromDims dims

singlePositionsToVec :: Dims -> Positions -> Vec
singlePositionsToVec = undefined

toMaybePositionsOfChar :: Char -> Positions -> [Maybe Char]
toMaybePositionsOfChar selectedChar inStr = undefined


-- World
-- Knows how World is implemented START
data World = World {fromWorld :: String, fromWorldDims :: Dims} deriving (Show, Eq, Ord)
toWorld = World
  where worldStr



blankWorld :: Dims -> World
blankWorld dims = World $ unlines (replicate height (replicate width '#'))
  where (width,height) = fromDims dims

overlay :: Dims -> World -> [Maybe Char] -> World
overlay = undefined

readPositionsOfChar :: Dims -> Char -> World -> Positions
readPositionsOfChar dims selectedChar (World inStr) = unionPositions positions
  where positions :: [Positions]
        positions = do
            let rows = lines inStr
            (y,row)  <- zip [0..] rows
            (x,char) <- zip [0..] row
            
            guard $ char == selectedChar
            
            return $ vecToSinglePositions dims (toVec (x,y))

-- Dims
-- Knows how Dims is implemented START
newtype Dims = Dims {fromDims :: (Int,Int)} deriving (Show, Eq, Ord)
readDims :: World -> Dims
readDims (World worldStr) = Dims (width, height)
  where rows = lines worldStr
        height = length rows
        width = length $ head rows
-- Knows how Dims is implemented END
-- Knows how World is implemented END


-- Read
readStartAndReachable :: World -> (Positions, Positions, Dims)
readStartAndReachable worldStr = (start, reachable, dims)
  where reachable = readPositionsOfChar dims '.' worldStr
        start = readPositionsOfChar dims 'S' worldStr
        dims = readDims worldStr


-- Show
showPositions :: (Char, Char, Positions, Dims) -> World
showPositions (emptyChar, filledChar, ps, dims) = blankWorld `overlay`

showStartAndReachable :: (Positions, Positions, Dims) -> World
showStartAndReachable (start, reachable, dims) = undefined

showStartAndReachableAndReached :: (Positions, Positions, Positions, Dims) -> World
showStartAndReachableAndReached (start, reachable, reached, dims) = undefined


-- Print
printPositions = putStrLn . fromWorld . showPositions
printStartAndReachable = putStrLn . fromWorld . showStartAndReachable
printReachableAndReached = putStrLn . fromWorld . showStartAndReachableAndReached


-- Main
day21part1 = do
    contents <- readFile "day21 (example).csv"
    let (start, reachable, dims) = readStartAndReachable (toWorld contents)
    
    printStartAndReachable (start, reachable, dims)
