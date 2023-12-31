#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19

module WalkableBoundedWorld (WalkableBoundedWorld(WalkableBoundedWorld), charOrder, addRocksToRightAndTop) where

-------------
-- Imports --
-------------
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Data.Ord

import Layer ( allDirs )

import World as W ( World(..)
                  , readWorld
                  , showWorld
                  , combineWorlds
                  , moveLayerInWorld
                  , cutLayerWithLayer
                  , insertLayerAtPoint )

import WalkableWorld

newtype WalkableBoundedWorld = WalkableBoundedWorld {coreWorld :: World}

instance WalkableWorld WalkableBoundedWorld where
    -- Assumes all rows have equal length
    readWorld :: String -> (Int, WalkableBoundedWorld)
    readWorld = fmap WalkableBoundedWorld . W.readWorld '.' ['S'] . addRocksToRightAndTop

    showWorld :: Int -> WalkableBoundedWorld -> String
    showWorld height w = W.showWorld height charOrder (WalkableWorld.coreWorld w)

    printWorld :: Int -> WalkableBoundedWorld -> IO ()
    printWorld height w = putStrLn $ WalkableWorld.showWorld height w

    removeForbidden :: WalkableBoundedWorld -> WalkableBoundedWorld
    removeForbidden w = WalkableBoundedWorld $ cutLayerWithLayer 'O' '#' (WalkableWorld.coreWorld w)

    progressByAStep :: WalkableBoundedWorld -> WalkableBoundedWorld
    progressByAStep w = WalkableWorld.removeForbidden . WalkableBoundedWorld $ combineWorlds $ map (\dir -> moveLayerInWorld 'O' dir (WalkableWorld.coreWorld w)) allDirs

    setOAtS :: WalkableBoundedWorld -> WalkableBoundedWorld
    setOAtS = WalkableBoundedWorld . fromJust . insertLayerAtPoint 'O' 'S' . WalkableWorld.coreWorld
    
    coreWorld :: WalkableBoundedWorld -> W.World
    coreWorld = WalkableBoundedWorld.coreWorld

charOrder :: Char -> Char -> Ordering
charOrder c1 c2 = comparing specialRank c1 c2 <> compare c1 c2
  where compareSpecial = comparing specialRank
        
        specialRank c = findIndex (==c) ['O','S','#','.']

addRocksToRightAndTop :: String -> String
addRocksToRightAndTop inStr = unlines . (\rows -> map (const '#') (head rows) : rows) . map (++"#") . lines $ inStr