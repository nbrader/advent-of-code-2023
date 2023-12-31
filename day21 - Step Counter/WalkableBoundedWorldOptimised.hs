#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19

module WalkableBoundedWorldOptimised (WalkableBoundedWorldOptimised(WalkableBoundedWorldOptimised), charOrder, addRocksToRightAndTop) where

-------------
-- Imports --
-------------
import Data.List (findIndex)
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.Ord
import Data.Bits
import Control.Monad (guard)

import Layer ( allDirs )

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

data WalkableBoundedWorldOptimised
    = WalkableBoundedWorldOptimised
            { worldBGChar :: Char
            , worldWalls :: S.Set (Int,Int)
            , worldStart :: (Int,Int)
            , worldPrevVisited :: S.Set (Int,Int)
            , worldPrevPrevVisited :: S.Set (Int,Int)
            , worldCountedOddSteps :: Integer
            , worldCountedEvenSteps :: Integer
            , worldWidth :: Int } deriving (Show)

instance WalkableWorld WalkableBoundedWorldOptimised where
    -- Assumes all rows have equal length
    readWorld :: String -> (Int,WalkableBoundedWorldOptimised)
    readWorld inStr
        = ( height
          , WalkableBoundedWorldOptimised
                { worldBGChar = bgChar
                , worldWalls = S.fromList . map snd . filter ((==startChar) . fst) $ char2Ds
                , worldStart = head       . map snd . filter ((==wallChar)  . fst) $ char2Ds
                , worldPrevVisited = S.empty
                , worldPrevPrevVisited = S.empty
                , worldCountedOddSteps = 0
                , worldCountedEvenSteps = 0
                , worldWidth = width } )
      
      where boundedInStr = boundWorldAsString inStr
            rows = lines boundedInStr
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

    showWorld :: Int -> WalkableBoundedWorldOptimised -> String
    showWorld height w = undefined -- W.showWorld height charOrder (Class.asWorld w)

    removeForbidden :: WalkableBoundedWorldOptimised -> WalkableBoundedWorldOptimised
    removeForbidden w = undefined -- WalkableBoundedWorldOptimised $ cutLayerWithLayer 'O' '#' (Class.asWorld w)

    progressByAStep :: WalkableBoundedWorldOptimised -> WalkableBoundedWorldOptimised
    progressByAStep w = undefined -- removeForbidden . WalkableBoundedWorldOptimised $ combineWorlds $ map (\dir -> moveLayerInWorld 'O' dir (Class.asWorld w)) allDirs

    setOAtS :: WalkableBoundedWorldOptimised -> WalkableBoundedWorldOptimised
    setOAtS w = w {worldPrevVisited = S.singleton (worldStart w)}
    
    asWorld :: WalkableBoundedWorldOptimised -> W.World
    asWorld = undefined -- WalkableBoundedWorldOptimised.asWorld
    
    oCount :: WalkableBoundedWorldOptimised -> Int
    oCount = undefined -- popCount . fromJust . M.lookup 'O' . worldLayers . Class.asWorld

bgChar = '.'
startChar = 'S'
wallChar = '#'
recentVisitChar = 'O'

boundWorldAsString :: String -> String
boundWorldAsString inStr = unlines . (\rows -> let wallRow = map (const '#') (head rows) in wallRow : rows ++ [wallRow]) . map (\row -> '#':row++"#") . lines $ inStr