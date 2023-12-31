#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19

module WalkableBoundedWorldOptimised (WalkableBoundedWorldOptimised(WalkableBoundedWorldOptimised), charOrder, addRocksToRightAndTop) where

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

import WalkableBoundedWorld as B
                            ( charOrder
                            , addRocksToRightAndTop )

newtype WalkableBoundedWorldOptimised = WalkableBoundedWorldOptimised {asWorld :: World}

instance WalkableWorld WalkableBoundedWorldOptimised where
    -- Assumes all rows have equal length
    readWorld :: String -> (Int, WalkableBoundedWorldOptimised)
    readWorld = undefined -- fmap WalkableBoundedWorldOptimised . W.readWorld '.' ['S'] . addRocksToRightAndTop

    showWorld :: Int -> WalkableBoundedWorldOptimised -> String
    showWorld height w = undefined -- W.showWorld height charOrder (Class.asWorld w)

    removeForbidden :: WalkableBoundedWorldOptimised -> WalkableBoundedWorldOptimised
    removeForbidden w = undefined -- WalkableBoundedWorldOptimised $ cutLayerWithLayer 'O' '#' (Class.asWorld w)

    progressByAStep :: WalkableBoundedWorldOptimised -> WalkableBoundedWorldOptimised
    progressByAStep w = undefined -- removeForbidden . WalkableBoundedWorldOptimised $ combineWorlds $ map (\dir -> moveLayerInWorld 'O' dir (Class.asWorld w)) allDirs

    setOAtS :: WalkableBoundedWorldOptimised -> WalkableBoundedWorldOptimised
    setOAtS = undefined -- WalkableBoundedWorldOptimised . fromJust . insertLayerAtPoint 'O' 'S' . Class.asWorld
    
    asWorld :: WalkableBoundedWorldOptimised -> W.World
    asWorld = undefined -- WalkableBoundedWorldOptimised.asWorld
    
    oCount :: WalkableBoundedWorldOptimised -> Int
    oCount = undefined -- popCount . fromJust . M.lookup 'O' . worldLayers . Class.asWorld