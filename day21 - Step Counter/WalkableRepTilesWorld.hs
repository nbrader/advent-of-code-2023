#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

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

    -- progressByAStep expands the world with copies of the original world to allow indefinite walking in all directions
    progressByAStep :: WalkableRepTilesWorld -> WalkableRepTilesWorld
    progressByAStep w
        | expansionRequired = after
        | otherwise         = undefined -- Find out which directions need expanding and add copies of the original world to expand in that direction.
      where before = w
            after  = Class.progressByAStep w
            
            expansionRequired = undefined
            -- expansionRequired = Class.oCount before == Class.oCount after -- This is bad logic because the oCount would be changing due to going in multiple directions.
                                                                             -- I need to test whether the oCount reduces between
                                                                             --    (what it was just after spreading out)
                                                                             --  and
                                                                             --    (what it was just after culling only based on world bounds).
                                                                             -- Only after checking and ensuring adequate bounds size should I then remove all forbidden (which at that point should just be those that hit rocks).

fromWorldAndBounded :: (World, WalkableBoundedWorld) -> WalkableRepTilesWorld
fromWorldAndBounded (originalWorld,boundedWorld) = WalkableRepTilesWorld {asOriginalWorld = originalWorld, WalkableRepTilesWorld.asWorld = Class.asWorld boundedWorld}

toWorldAndBounded :: WalkableRepTilesWorld -> (World, WalkableBoundedWorld)
toWorldAndBounded w = (originalWorld, WalkableBoundedWorld coreWorld)
  where coreWorld = Class.asWorld w
        originalWorld = asOriginalWorld w

toBounded :: WalkableRepTilesWorld -> WalkableBoundedWorld
toBounded = WalkableBoundedWorld . Class.asWorld