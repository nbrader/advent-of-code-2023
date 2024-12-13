#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

module World ( World(..)
             , emptyWorld
             , readWorld
             , showWorld
             , printWorld
             , combineTwoWorlds
             , combineWorlds
             , hasPoint
             , moveLayerInWorld
             , movePointInWorld
             , cutLayerWithLayer
             , setPoint
             , insertLayerAtPoint
             , isOverlappingLayers ) where

-------------
-- Imports --
-------------
import Data.List (sortBy, groupBy, delete, find, transpose)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.Maybe (fromJust, catMaybes, fromMaybe)
import Data.Ord
import Data.Function
import Data.Bits
import Control.Monad (guard, join)
import Data.Monoid
import Data.Foldable
import Safe (atMay)

import Util (replace)
import Layer (SingularPoint, Layer, pointToIndex, pointToLayer, moveLayer, movePoint, isOverlapping, diff, up, dn, lt, rt, allDirs)

-- Each obj has a shape encoded as bits of an Integer.

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

isOverlappingLayers :: Char -> Char -> World -> Bool
isOverlappingLayers c1 c2 w
    = fromMaybe False $ do
        points1 <- M.lookup c1 (worldLayers w)
        points2 <- M.lookup c2 (worldLayers w)
        
        return $ points1 `isOverlapping` points2
