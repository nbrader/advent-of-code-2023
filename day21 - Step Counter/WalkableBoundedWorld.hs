#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19

module WalkableBoundedWorld where

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

-- Assumes all rows have equal length
readWorld :: String -> (Int, W.World)
readWorld = W.readWorld '.' ['S'] . addRocksToRightAndTop

showWorld :: Int -> W.World -> String
showWorld height world = W.showWorld height charOrder world

printWorld :: Int -> W.World -> IO ()
printWorld height world = putStrLn $ WalkableBoundedWorld.showWorld height world

charOrder :: Char -> Char -> Ordering
charOrder c1 c2 = comparing specialRank c1 c2 <> compare c1 c2
  where compareSpecial = comparing specialRank
        
        specialRank c = findIndex (==c) ['O','S','#','.']

addRocksToRightAndTop :: String -> String
addRocksToRightAndTop inStr = unlines . (\rows -> map (const '#') (head rows) : rows) . map (++"#") . lines $ inStr

removeForbidden :: W.World -> W.World
removeForbidden w = cutLayerWithLayer 'O' '#' w

progressByAStep :: W.World -> W.World
progressByAStep w = removeForbidden $ combineWorlds $ map (\dir -> moveLayerInWorld 'O' dir w) allDirs

setOAtS = fromJust . insertLayerAtPoint 'O' 'S'