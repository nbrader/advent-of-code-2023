#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

module WalkableBoundedWorld (WalkableBoundedWorld(WalkableBoundedWorld), charOrder, addRocksToRightAndTop) where

-------------
-- Imports --
-------------
import Data.List (findIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Ord
import Data.Bits

import Layer ( allDirs )

import World as W ( World(..)
                  , readWorld
                  , showWorld
                  , combineWorlds
                  , moveLayerInWorld
                  , cutLayerWithLayer
                  , insertLayerAtPoint )

import WalkableWorld as Class

newtype WalkableBoundedWorld = WalkableBoundedWorld {asWorld :: World}

instance WalkableWorld WalkableBoundedWorld where
    -- Assumes all rows have equal length
    readWorld :: String -> (Int, WalkableBoundedWorld)
    readWorld = fmap WalkableBoundedWorld . W.readWorld '.' ['S'] . addRocksToRightAndTop

    showWorld :: Int -> WalkableBoundedWorld -> String
    showWorld height w = W.showWorld height charOrder (Class.asWorld w)

    removeForbidden :: WalkableBoundedWorld -> WalkableBoundedWorld
    removeForbidden w = WalkableBoundedWorld $ cutLayerWithLayer 'O' '#' (Class.asWorld w)

    progressByAStep :: WalkableBoundedWorld -> WalkableBoundedWorld
    progressByAStep w = removeForbidden . WalkableBoundedWorld $ combineWorlds $ map (\dir -> moveLayerInWorld 'O' dir (Class.asWorld w)) allDirs

    setOAtS :: WalkableBoundedWorld -> WalkableBoundedWorld
    setOAtS = WalkableBoundedWorld . fromJust . insertLayerAtPoint 'O' 'S' . Class.asWorld
    
    asWorld :: WalkableBoundedWorld -> W.World
    asWorld = WalkableBoundedWorld.asWorld
    
    oCount :: WalkableBoundedWorld -> Integer
    oCount = toInteger . popCount . fromJust . M.lookup 'O' . worldLayers . Class.asWorld

charOrder :: Char -> Char -> Ordering
charOrder c1 c2 = comparing specialRank c1 c2 <> compare c1 c2
  where compareSpecial = comparing specialRank
        
        specialRank c = findIndex (==c) ['O','S','#','.']

addRocksToRightAndTop :: String -> String
addRocksToRightAndTop inStr = unlines . (\rows -> map (const '#') (head rows) : rows) . map (++"#") . lines $ inStr