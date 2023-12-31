#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19

module WalkableRepTilesWorld (WalkableRepTilesWorld(WalkableRepTilesWorld)) where

-------------
-- Imports --
-------------
import Data.List (findIndex)
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

import WalkableBoundedWorld as B
                            ( charOrder
                            , addRocksToRightAndTop )

import WalkableWorld as Class

data WalkableRepTilesWorld = WalkableRepTilesWorld {coreWorld :: World}

instance WalkableWorld WalkableRepTilesWorld where
    readWorld :: String -> (Int,WalkableRepTilesWorld)
    readWorld = fmap WalkableRepTilesWorld . W.readWorld '.' ['S'] . addRocksToRightAndTop

    showWorld :: Int -> WalkableRepTilesWorld -> String
    showWorld height world = W.showWorld height charOrder (Class.coreWorld world)

    removeForbidden :: WalkableRepTilesWorld -> WalkableRepTilesWorld
    removeForbidden w = WalkableRepTilesWorld $ cutLayerWithLayer 'O' '#' (Class.coreWorld w)

    progressByAStep :: WalkableRepTilesWorld -> WalkableRepTilesWorld
    progressByAStep w = undefined --removeForbidden $ WalkableRepTilesWorld $ combineWorlds $ map (\dir -> moveLayerInWorld 'O' dir (Class.coreWorld w)) allDirs

    setOAtS :: WalkableRepTilesWorld -> WalkableRepTilesWorld
    setOAtS w = WalkableRepTilesWorld $ fromJust . insertLayerAtPoint 'O' 'S' $ Class.coreWorld w

    coreWorld :: WalkableRepTilesWorld -> W.World
    coreWorld = WalkableRepTilesWorld.coreWorld