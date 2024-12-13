#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package linear-1.22

-----------------------------------
-----------------------------------
----  Day 18: Lavaduct Lagoon  ----
-----------------------------------
-----------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package linear-1.22 -- '.\day18.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day18part1
-- 50603

-- *Main> day18part2
-- 


-------------
-- Imports --
-------------
import Data.List (foldl', nub, sortBy, groupBy, delete)
import Data.Maybe (catMaybes)
import Linear hiding (trace)
import Data.Ord
import Data.Function


-------------
-- Program --
-------------
main = day18part1

data Instruction = Instruction {instructionDir :: V2 Int, instructionLength :: Int} deriving (Show)

vUp = V2 ( 0) (-1)
vDn = V2 ( 0) ( 1)
vLt = V2 (-1) ( 0)
vRt = V2 ( 1) ( 0)

getX (V2 x y) = x
getY (V2 x y) = y

rot90 :: V2 Int -> V2 Int
rot90 (V2 x y) = V2 (-y) x

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

readInstructions :: String -> [Instruction]
readInstructions inStr = do
    row <- rows
    let [dirStr, lenStr, hexStr] = words row
    let dir = readDirStr dirStr
    let len = read lenStr
    return $ Instruction dir len
  where rows = lines inStr

readInstructions2 :: String -> [Instruction]
readInstructions2 inStr = do
    row <- rows
    let [dirStr, lenStr, hexStr] = words row
    let len = readHex . tail . tail . init $ hexStr
    let dir = readDirDigit . last . init $ hexStr
    return $ Instruction dir len
  where rows = lines inStr

start = V2 0 0

pathFromStart :: [Instruction] -> [V2 Int]
pathFromStart = foldl' step [start]
  where step (v:visited) (Instruction dir len) = (v + len*^dir):v:visited

type LinearPath = (V2 Int, V2 Int)

-- toLinearPaths :: [V2 Int] -> [LinearPath]
-- toLinearPaths path = zip (init path) (tail path)

toLinearPaths :: [V2 Int] -> [LinearPath]
toLinearPaths path = catMaybes $ map (\linearPath -> if horizontal linearPath then trimHorizontalEnds linearPath else Just linearPath) $ zip (init path) (tail path)

trimHorizontalEnds :: LinearPath -> Maybe LinearPath
trimHorizontalEnds linearPath@(V2 startX startY, V2 endX endY)
    | xDisp == 1 = Nothing
    | otherwise  = Just (V2 (startX+dir) startY, V2 (endX-dir) endY)
  where xDisp = getLinearPathXDisp linearPath
        dir = signum $ xDisp

horizontal :: LinearPath -> Bool
horizontal (start, end) = getY start == getY end

vertical :: LinearPath -> Bool
vertical (start, end) = getX start == getX end

type Interval = (Int,Int)
projectToX :: LinearPath -> Interval
projectToX ((V2 startX _),(V2 endX _)) = (startX, endX)

toIncreasing :: Interval -> Interval
toIncreasing (start,end) = (min start end, max start end)

getLinearPathXDisp (start,end) = getX start - getX end
getLinearPathYDisp (start,end) = getX start - getX end

getLinearPathXSize = abs . getLinearPathXDisp
getLinearPathYSize = abs . getLinearPathYDisp

-- linearPathsToInternalArea :: [LinearPath] -> Int
linearPathsToInternalArea linearPaths = pathGroups --sum groupAreas
  where verticalPaths = filter vertical linearPaths
        sortedVerticals@(leftMostVerticalPath:_) = sortBy (comparing (getX . fst)) $ verticalPaths
        (leftMostStart, leftMostEnd) = leftMostVerticalPath
        isClockwise = getY leftMostEnd - getY leftMostStart < 0
        refinedPaths = pathsPartitionHoriz linearPaths
        pathGroups = groupBy ((==) `on` (toIncreasing . projectToX)) . sortBy (comparing (toIncreasing . projectToX)) $ refinedPaths
        groupAreas = map area pathGroups
        area pathGroup
            | width == 0 = undefined $ pathGroup
          where firstLine = head pathGroup
                width = getLinearPathXSize firstLine

pathsPartitionHoriz :: [LinearPath] -> [LinearPath]
pathsPartitionHoriz linearPaths = do
    originalPath <- linearPaths
    refineLinearPathByPaths originalPath (delete originalPath linearPaths)

refineLinearPathByPaths :: LinearPath -> [LinearPath] -> [LinearPath]
refineLinearPathByPaths originalPath linearPaths = foldl' (\partitionOfOriginal other -> concat $ map (refineLinearPathByLinearPath other) partitionOfOriginal) [originalPath] linearPaths

makeRightward :: LinearPath -> LinearPath
makeRightward linearPath
    | isRightward linearPath = flipLinearPath linearPath
    | otherwise              =                linearPath

isRightward :: LinearPath -> Bool
isRightward (start,end) = getX end >= getX start

flipLinearPath :: LinearPath -> LinearPath
flipLinearPath (start,end) = (end,start)

refineLinearPathByLinearPath :: LinearPath -> LinearPath -> [LinearPath]
refineLinearPathByLinearPath applied original
    | isRightward original =                      refineRightwardLinearPathByRightwardLinearPath rightwardApplied original
    | otherwise            = map flipLinearPath $ refineRightwardLinearPathByRightwardLinearPath rightwardApplied (makeRightward original)
  where rightwardApplied = makeRightward applied

refineRightwardLinearPathByRightwardLinearPath :: LinearPath -> LinearPath -> [LinearPath]
refineRightwardLinearPathByRightwardLinearPath (appliedStart, appliedEnd) (originalStart, originalEnd)
    | originalEndX   <  appliedStartX || originalStartX >  appliedEndX = [(originalStart, originalEnd)]
    | originalStartX <  appliedStartX && originalEndX   <= appliedEndX = [(originalStart, appliedStartSubOneCut), (appliedStartCut, originalEnd)]
    | originalStartX <  appliedStartX && originalEndX   >  appliedEndX = [(originalStart, appliedStartSubOneCut), (appliedStartCut, appliedEndCut), (appliedEndPlusOneCut, originalEnd)]
    | originalStartX == appliedStartX && originalEndX   <= appliedEndX = [(appliedStartCut, originalEnd)]
    | originalStartX == appliedStartX && originalEndX   >  appliedEndX = [(appliedStartCut, appliedEndCut), (appliedEndPlusOneCut, originalEnd)]
    | originalStartX >  appliedStartX && originalEndX   <= appliedEndX = [(originalStart, originalEnd)]
    | otherwise                                                        = [(originalStart, appliedEndCut), (appliedEndPlusOneCut, originalEnd)]
  where y = getY originalStart
        originalStartX = getX originalStart
        originalEndX   = getX originalEnd
        appliedStartX  = getX appliedStart
        appliedEndX    = getX appliedEnd
        appliedStartSubOneCut = V2 (appliedStartX-1) y
        appliedStartCut       = V2  appliedStartX    y
        appliedEndCut         = V2  appliedEndX      y
        appliedEndPlusOneCut  = V2 (appliedEndX+1)   y

-- refineLinearPathByPaths originalPath linearPaths = foldr 

-- refineLinearPathByPath originalPath linearPaths = foldr []

day18part1 = do
  contents <- readFile "day18 (example).csv"
  let instructions = readInstructions2 contents
  let linearPaths = toLinearPaths . pathFromStart $ instructions
  -- mapM_ print linearPaths
  -- putStrLn ""
  mapM_ print $ linearPathsToInternalArea linearPaths
  -- putStrLn ""
  -- mapM_ print $ map (map projectToX) $ linearPathsToInternalArea linearPaths
  -- putStrLn ""
  -- mapM_ print $ map (map (toIncreasing . projectToX)) $ linearPathsToInternalArea linearPaths

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
