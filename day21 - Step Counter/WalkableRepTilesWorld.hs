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
                            ( WalkableBoundedWorld(..)
                            , charOrder
                            , addRocksToRightAndTop )

import WalkableWorld as Class

data WalkableRepTilesWorld = WalkableRepTilesWorld {coreWorld :: World}

instance WalkableWorld WalkableRepTilesWorld where
    readWorld        = fmap fromBounded . Class.readWorld
    showWorld height =                    Class.showWorld height . toBounded
    removeForbidden  =      fromBounded . Class.removeForbidden  . toBounded
    setOAtS          =      fromBounded . Class.setOAtS          . toBounded
    coreWorld        =                    Class.coreWorld        . toBounded

    progressByAStep :: WalkableRepTilesWorld -> WalkableRepTilesWorld
    progressByAStep w = undefined --removeForbidden $ WalkableRepTilesWorld $ combineWorlds $ map (\dir -> moveLayerInWorld 'O' dir (Class.coreWorld w)) allDirs



fromBounded :: WalkableBoundedWorld -> WalkableRepTilesWorld
fromBounded = WalkableRepTilesWorld . Class.coreWorld

toBounded :: WalkableRepTilesWorld -> WalkableBoundedWorld
toBounded = WalkableBoundedWorld . Class.coreWorld