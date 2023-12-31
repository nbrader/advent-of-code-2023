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

data WalkableRepTilesWorld = WalkableRepTilesWorld {asWorld :: World, asOriginalWorld :: World}

instance WalkableWorld WalkableRepTilesWorld where
    readWorld        = fmap (fromWorldAndBounded . (\x -> (Class.asWorld x, x))) . Class.readWorld
    removeForbidden  = fromWorldAndBounded . fmap Class.removeForbidden  . toWorldAndBounded
    setOAtS          = fromWorldAndBounded . fmap Class.setOAtS          . toWorldAndBounded
    showWorld height = Class.showWorld height . toBounded
    asWorld          = Class.asWorld          . toBounded
    oCount           = Class.oCount           . toBounded

    progressByAStep :: WalkableRepTilesWorld -> WalkableRepTilesWorld
    progressByAStep w
        | Class.oCount before == Class.oCount after = after
        | otherwise                                 = undefined -- find out which directions need expanding and add a copy of the original world
      where before = w
            after  = Class.progressByAStep w

fromWorldAndBounded :: (World, WalkableBoundedWorld) -> WalkableRepTilesWorld
fromWorldAndBounded (w,w') = WalkableRepTilesWorld w (Class.asWorld w')

toWorldAndBounded :: WalkableRepTilesWorld -> (World, WalkableBoundedWorld)
toWorldAndBounded w = (originalWorld, WalkableBoundedWorld coreWorld)
  where coreWorld = Class.asWorld w
        originalWorld = asOriginalWorld w

toBounded :: WalkableRepTilesWorld -> WalkableBoundedWorld
toBounded = WalkableBoundedWorld . Class.asWorld