#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19

module WalkableRepTilesWorldOptimised (WalkableRepTilesWorldOptimised(WalkableRepTilesWorldOptimised)) where

-------------
-- Imports --
-------------
import Data.List (findIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Ord

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
            ( World(..)
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
            , isOverlappingLayers )

import WalkableWorld as Class

import WalkableBoundedWorld as B
                            ( charOrder
                            , addRocksToRightAndTop )

data WalkableRepTilesWorldOptimised = WalkableRepTilesWorldOptimised {asWorld :: World, asOriginalWorld :: World}

instance WalkableWorld WalkableRepTilesWorldOptimised where
    -- Assumes all rows have equal length
    readWorld :: String -> (Int, WalkableRepTilesWorldOptimised)
    readWorld = undefined -- fmap WalkableRepTilesWorldOptimised . W.readWorld '.' ['S'] . addRocksToRightAndTop

    showWorld :: Int -> WalkableRepTilesWorldOptimised -> String
    showWorld height w = undefined -- W.showWorld height charOrder (Class.asWorld w)

    removeForbidden :: WalkableRepTilesWorldOptimised -> WalkableRepTilesWorldOptimised
    removeForbidden w = undefined -- WalkableRepTilesWorldOptimised $ cutLayerWithLayer 'O' '#' (Class.asWorld w)

    progressByAStep :: WalkableRepTilesWorldOptimised -> WalkableRepTilesWorldOptimised
    progressByAStep w = undefined -- removeForbidden . WalkableRepTilesWorldOptimised $ combineWorlds $ map (\dir -> moveLayerInWorld 'O' dir (Class.asWorld w)) allDirs

    setOAtS :: WalkableRepTilesWorldOptimised -> WalkableRepTilesWorldOptimised
    setOAtS = undefined -- WalkableRepTilesWorldOptimised . fromJust . insertLayerAtPoint 'O' 'S' . Class.asWorld
    
    asWorld :: WalkableRepTilesWorldOptimised -> W.World
    asWorld = undefined -- WalkableRepTilesWorldOptimised.asWorld
    
    oCount :: WalkableRepTilesWorldOptimised -> Integer
    oCount = undefined -- popCount . fromJust . M.lookup 'O' . worldLayers . Class.asWorld
