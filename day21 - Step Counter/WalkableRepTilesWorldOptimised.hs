#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

module WalkableRepTilesWorldOptimised (WalkableRepTilesWorldOptimised(WalkableRepTilesWorldOptimised)) where

-------------
-- Imports --
-------------
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.Ord
import Data.Bits
import Control.Monad (guard)

import Layer ( SingularPoint
             , Layer
             , pointToIndex
             , pointToLayer
             , moveLayer
             , movePoint
             , isOverlapping
             , diff
             , up
             , dn
             , lt
             , rt
             , allDirs )

import World as W
            ( World )

import WalkableWorld as Class

import WalkableBoundedWorld as B
                            ( charOrder
                            , addRocksToRightAndTop )

data WalkableRepTilesWorldOptimised
    = WalkableRepTilesWorldOptimised
            { worldBGChar :: Char
            , worldWalls :: S.Set (Int,Int)
            , worldStart :: (Int,Int)
            , worldPrevVisited :: S.Set (Int,Int)
            , worldPrevPrevVisited :: S.Set (Int,Int)
            , worldCountedOddSteps :: Integer
            , worldCountedEvenSteps :: Integer
            , worldStepCount :: Integer
            , worldWidth :: Int
            , worldHeight :: Int } deriving (Show)

instance WalkableWorld WalkableRepTilesWorldOptimised where
    -- Assumes all rows have equal length
    readWorld :: String -> (Int,WalkableRepTilesWorldOptimised)
    readWorld inStr
        = ( height
          , WalkableRepTilesWorldOptimised
                { worldBGChar = bgChar
                , worldWalls = S.fromList . map snd . filter ((==wallChar) . fst) $ char2Ds
                , worldStart = head       . map snd . filter ((==startChar)  . fst) $ char2Ds
                , worldPrevVisited = S.empty
                , worldPrevPrevVisited = S.empty
                , worldCountedOddSteps = 0
                , worldCountedEvenSteps = 0
                , worldStepCount = 0
                , worldWidth = width
                , worldHeight = height } )
      
      where rows = lines inStr
            height = length rows
            width
              | height == 0 = 0
              | otherwise   = length $ head rows
            char2Ds = readChar2DsFromRows rows
            
            readChar2DsFromRows :: [String] -> [(Char, (Int,Int))]
            readChar2DsFromRows rows = do
                (y',row) <- zip [0..] rows
                (x,char) <- zip [0..] row
                
                let y = height - 1 - y'
                
                return (char, (x, y))

    showWorld :: Int -> WalkableRepTilesWorldOptimised -> String
    showWorld height w = show (worldPrevVisited w) -- W.showWorld height charOrder (Class.asWorld w)

    removeForbidden :: WalkableRepTilesWorldOptimised -> WalkableRepTilesWorldOptimised
    removeForbidden w = undefined

    progressByAStep :: WalkableRepTilesWorldOptimised -> WalkableRepTilesWorldOptimised
    progressByAStep w = w { worldPrevVisited = newVisited
                          , worldPrevPrevVisited = worldPrevVisited w
                          , worldCountedOddSteps  = if not evenStep then worldCountedOddSteps w + numOfNewVisited else worldCountedOddSteps w
                          , worldCountedEvenSteps = if evenStep     then worldCountedOddSteps w + numOfNewVisited else worldCountedOddSteps w
                          , worldStepCount = worldStepCount w + 1 } -- WalkableRepTilesWorldOptimised $ combineWorlds $ map (\dir -> moveLayerInWorld 'O' dir (Class.asWorld w)) allDirs
      where newVisitedIgnoringWalls = S.unions $ map (\(dx,dy) -> S.map (\(x,y) -> (x+dx,y+dy)) (worldPrevVisited w)) allDirs
            newVisited = S.filter (\(x,y) -> let normalizedPosition = (x `mod` worldWidth w, y `mod` worldHeight w) in not (normalizedPosition `S.member` worldWalls w)) . S.filter (not . (`S.member` worldPrevPrevVisited w)) $ newVisitedIgnoringWalls
            numOfNewVisited = toInteger $ S.size newVisited
            newStepCount = worldStepCount w + 1
            evenStep = worldStepCount w `mod` 2 == 0

    setOAtS :: WalkableRepTilesWorldOptimised -> WalkableRepTilesWorldOptimised
    setOAtS w = w {worldPrevVisited = S.singleton (worldStart w), worldPrevPrevVisited = S.empty, worldCountedOddSteps = 1, worldCountedEvenSteps = 0}
    
    asWorld :: WalkableRepTilesWorldOptimised -> W.World
    asWorld = undefined -- WalkableBoundedWorldOptimised.asWorld
    
    oCount :: WalkableRepTilesWorldOptimised -> Integer
    oCount = worldCountedOddSteps

bgChar = '.'
startChar = 'S'
wallChar = '#'
recentVisitChar = 'O'