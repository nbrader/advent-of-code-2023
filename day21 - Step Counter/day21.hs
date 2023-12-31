#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22 --package split-0.2.3.5 --package deque-0.4.4.1 --package safe-0.3.19

--------------------------------
--------------------------------
----  Day 21: Step Counter  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package linear-1.22 --package deque-0.4.4.1 --package safe-0.3.19 -- '.\day21.hs' -O2
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
import Data.List (foldl', nub, sort, sortBy, groupBy, delete, find, transpose, findIndex)
import Data.List.Split (splitOn, chunksOf)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybeToList, catMaybes, fromMaybe)
import Linear hiding (trace, transpose)
import Linear.V2
import Debug.Trace (trace)
import Data.Ord
import Data.Function
import Data.Tuple
import GHC.Exts as F
import Data.Bits
import Control.Monad (guard, join)
import Data.Monoid
import Data.Foldable
import Data.Ord
import Safe (atMay)


-------------
-- Program --
-------------
main = day21part2


-- Each obj has a shape encoded as bits of an Integer.
type SingularPoint = (Int,Int)
type Layer = Integer

data World
    = World { worldBG :: Char
            , worldLayers :: M.Map Char Layer
            , worldPoints :: M.Map Char SingularPoint
            , worldWidth :: Int } deriving (Show)

emptyWorld :: Char -> Int -> World
emptyWorld bgChar width = World bgChar mempty mempty width


-- Assumes all rows have equal length
readWorld :: Char -> [Char] -> String -> (Int,World)
readWorld bgChar singularChars inStr
    = ( height
      , World { worldBG = bgChar,
                worldLayers = foldr addToLayer M.empty $ filter (\(char,_) -> not (char `elem` singularChars)) char2Ds,
                worldPoints = singularPoints,
                worldWidth = width } )
       
  where rows = lines inStr
        height = length rows
        width
          | height == 0 = 0
          | otherwise   = length $ head rows
        char2Ds = readChar2DsFromRows rows
        singularPoints = M.fromList . catMaybes
                                    . map (\c -> find (\(c', (x,y)) -> c' == c) char2Ds)
                                    $ singularChars
        
        readChar2DsFromRows :: [String] -> [(Char, (Int,Int))]
        readChar2DsFromRows rows = do
            (y',row) <- zip [0..] rows
            (x,char) <- zip [0..] row
            
            let y = height - 1 - y'
            
            guard $ char /= bgChar
            
            return (char, (x, y))

        addToLayer :: (Char, (Int, Int)) -> M.Map Char Layer -> M.Map Char Layer
        addToLayer (char, (x, y)) = M.alter (setBitInLayer (x, y)) char

        setBitInLayer :: (Int, Int) -> Maybe Layer -> Maybe Layer
        setBitInLayer (x, y) maybeLayer = Just $ setBit (fromMaybe 0 maybeLayer) (y * width + x)


showWorld :: Int -> (Char -> Char -> Ordering) -> World -> String
showWorld height charZOrder world = unlines . reverse . take height . chunksOf width . map (fromMaybe bgChar) $ listOfMaybeCharsFromLayersAndPoints
  where (World bgChar layers points width) = world
        listsOfMaybeCharsFromLayers = prioritize charZOrder $ M.mapWithKey (\c n -> layerToMaybeChars c n) layers
        listOfMaybeCharsFromLayers = combineMaybeCharLists listsOfMaybeCharsFromLayers
        charsAndPoints = map head . groupBy ((==) `on` snd) . sortBy (\(aChar,aPos) (bChar,bPos) -> compare aPos bPos <> compare aChar bChar) . M.toList $ points
        charsAndIndices = map (fmap (pointToIndex width)) charsAndPoints
        listOfMaybeCharsFromLayersAndPoints = foldr (\(c,i) acc -> let maybeOld = join (acc `atMay` i) in replace acc (i, Just $ minimum $ catMaybes [maybeOld, Just c])) listOfMaybeCharsFromLayers charsAndIndices
        
        combineMaybeCharLists :: [[Maybe a]] -> [Maybe a]
        combineMaybeCharLists = map (getFirst . fold . map First) . transpose
        
        prioritize :: Ord a1 => (a1 -> a1 -> Ordering) -> M.Map a1 a2 -> [a2]
        prioritize charZOrder = (\m -> map (fromJust . flip M.lookup m) (sortBy charZOrder $ M.keys m))
        
        layerToMaybeChars :: Char -> Integer -> [Maybe Char]
        layerToMaybeChars c n = map (\i -> if n `testBit` i then Just c else Nothing) [0..]

replace [] _ = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) =
  if n < 0
    then (x:xs)
    else x: replace xs (n-1,a)

printWorld :: Int -> (Char -> Char -> Ordering) -> World -> IO ()
printWorld height charZOrder world = putStrLn $ showWorld height charZOrder world


-- Testing
exampleWorld1 :: World
exampleWorld1 = World '.' (M.fromList [('U',9),('Z',1)]) (M.fromList [('V',(1,0))]) 10

exampleWorld2 :: World
exampleWorld2 = World '.' (M.fromList [('U',20),('Z',1)]) (M.fromList [('V',(3,3))]) 10

exampleWorld3 :: World
exampleWorld3 = moveLayerInWorld 'U' (-1,-1) $ exampleWorld2

exampleWorld4 :: World
exampleWorld4 = exampleWorld1 `combineTwoWorlds` exampleWorld2

examplePrint1 = printWorld 10 (comparing id) exampleWorld1
examplePrint2 = printWorld 10 (comparing id) exampleWorld2
examplePrint3 = printWorld 10 (comparing id) exampleWorld3
examplePrint4 = printWorld 10 (comparing id) exampleWorld4


-- Assumes worlds are same size
-- Left-biased such that the background character and any singular points they share are taken from the left
combineTwoWorlds :: World -> World -> World
combineTwoWorlds w1 w2
    = w1 { worldLayers = M.unionWith combineLayers (worldLayers w1) (worldLayers w2),
           worldPoints = M.unionWith combinePoints (worldPoints w1) (worldPoints w2) }
  where combineLayers :: Layer -> Layer -> Layer
        combineLayers points1 points2 = points1 .|. points2
        
        combinePoints :: SingularPoint -> SingularPoint -> SingularPoint
        combinePoints point1 _ = point1

combineWorlds :: [World] -> World
combineWorlds = foldr1 combineTwoWorlds

hasPoint :: Char -> SingularPoint -> World -> Bool
hasPoint char point world = inSingularPoints || inLayers
  where
    inSingularPoints = case M.lookup char (worldPoints world) of
        Just p -> p == point
        Nothing -> False

    inLayers = case M.lookup char (worldLayers world) of
        Just bits -> testBit bits (pointToIndex (worldWidth world) point)
        Nothing -> False

-- Converts a 2D point to a 1D index
pointToIndex :: Int -> SingularPoint -> Int
pointToIndex width (x, y) = y * width + x

moveLayerInWorld :: Char -> (Int,Int) -> World -> World
moveLayerInWorld c (dx,dy) w = w {worldLayers = M.update (\pts -> Just $ moveLayer width (dx,dy) pts) c (worldLayers w)}
  where width = worldWidth w

movePointInWorld :: Char -> (Int,Int) -> World -> World
movePointInWorld c (dx,dy) w = w {worldPoints = M.update (\pt -> Just $ movePoint width (dx,dy) pt) c (worldPoints w)}
  where width = worldWidth w

cutLayerWithLayer :: Char -> Char -> World -> World
cutLayerWithLayer targetChar cuttingChar w
    |   targetChar  `M.member` worldLayers w
     && cuttingChar `M.member` worldLayers w = w {worldLayers = M.insert targetChar newLayer (worldLayers w)}
    | otherwise = w
  where targetLayer  = fromJust $ M.lookup targetChar  (worldLayers w)
        cuttingLayer = fromJust $ M.lookup cuttingChar (worldLayers w)
        newLayer = targetLayer `diff` cuttingLayer

setPoint :: Char -> (Int,Int) -> World -> World
setPoint c (x,y) w = w {worldPoints = M.insert c (x,y) (worldPoints w)}

insertLayerAtPoint :: Char -> Char -> World -> Maybe World
insertLayerAtPoint layerChar pointChar w = do
    point <- M.lookup pointChar (worldPoints w)
    let newLayer = pointToLayer width point
    return $ w {worldLayers = M.insert layerChar newLayer (worldLayers w)}
  where width = worldWidth w

pointToLayer :: Int -> SingularPoint -> Layer
pointToLayer width (x,y) = moveLayer width (x,y) 1

moveLayer :: Int -> (Int,Int) -> Layer -> Layer
moveLayer width (dx,dy) pts = pts `shift` pointToIndex width (dx,dy)

movePoint :: Int -> (Int,Int) -> (Int,Int) -> (Int,Int)
movePoint width (dx,dy) (x,y) = (x+dx,y+dy)

isOverlappingLayers :: Char -> Char -> World -> Bool
isOverlappingLayers c1 c2 w
    = fromMaybe False $ do
        points1 <- M.lookup c1 (worldLayers w)
        points2 <- M.lookup c2 (worldLayers w)
        
        return $ points1 `isOverlapping` points2


-- isOverlapping n n
-- time:  O(n) (approx.) [0 < n < 10000000000, 0 < secs < 7.97]
-- space: O(n) (approx.) [0 < n < 10000000000, 0 < bytes < 30000141704]
-- https://nux-pc/svn/Nux-SVN/My Programming/Scratchings/Advent of Code 2022/day17 (bits).hs/?r=1569
isOverlapping :: Layer -> Layer -> Bool
isOverlapping ps1 ps2 = (ps1 .&. ps2) /= zeroBits

diff :: Layer -> Layer -> Layer
diff ps1 ps2 = (ps1 .&. complement ps2)


up = (  0 ,   1 )
dn = (  0 , (-1))
lt = ((-1),   0 )
rt = (  1 ,   0 )
allDirs = [up,dn,lt,rt]

charOrder :: Char -> Char -> Ordering
charOrder c1 c2 = comparing specialRank c1 c2 <> compare c1 c2
  where compareSpecial = comparing specialRank
        
        specialRank c = findIndex (==c) ['O','S','#','.']

addRocksToRightAndTop :: String -> String
addRocksToRightAndTop inStr = unlines . (\rows -> map (const '#') (head rows) : rows) . map (++"#") . lines $ inStr

removeForbidden :: World -> World
removeForbidden w = cutLayerWithLayer 'O' '#' w

progressByAStep :: World -> World
progressByAStep w = removeForbidden $ combineWorlds $ map (\dir -> moveLayerInWorld 'O' dir w) allDirs

-- Main
day21part1 = do
    contents <- readFile "day21 (data).csv"
    let (height, world) = readWorld '.' ['S'] (addRocksToRightAndTop contents)
    let worldBeforeStep = fromJust $ insertLayerAtPoint 'O' 'S' world
    let futureWorlds = iterate progressByAStep worldBeforeStep
    -- print width
    -- print world
    -- mapM_ (printWorld 12 charOrder) (take 7 futureWorlds)
    print . popCount . fromJust . M.lookup 'O' . worldLayers . (!!64) $ futureWorlds

duplicateWorldNxN :: Int -> String -> String
duplicateWorldNxN n inStr = unlines . concat . replicate n . map (concat . replicate n) . lines $ inStr

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x `seq` (x : iterate' f (f x))

day21part2 = do
    contents <- readFile "day21 (example).csv"
    let (originalHeight, originalWorld) = readWorld '.' ['S'] contents
    let dupeCount = 2*((26501365 `div` originalHeight) + 1)
    let semiDupeCount = dupeCount `div` 2
    let originalWidth = worldWidth originalWorld
    let (height, world') = readWorld '.' ['S'] (duplicateWorldNxN dupeCount contents)
    let world = movePointInWorld 'S' (semiDupeCount*originalWidth,-semiDupeCount*originalHeight) world'
    let worldBeforeStep = fromJust $ insertLayerAtPoint 'O' 'S' world
    let futureWorlds = iterate' progressByAStep worldBeforeStep
    -- print width
    -- print world
    -- mapM_ (printWorld 150 charOrder) (take 7 futureWorlds)
    print . popCount . fromJust . M.lookup 'O' . worldLayers . (!!26501365) $ futureWorlds
