#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19

module WalkableRepTilesWorld where

-------------
-- Imports --
-------------
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
                            ( readWorld
                            , showWorld
                            , printWorld
                            , charOrder
                            , addRocksToRightAndTop
                            , removeForbidden
                            , progressByAStep
                            , setOAtS )

data WalkableRepTilesWorld = WalkableRepTilesWorld {coreWorld :: World}

readWorld :: String -> (Int,WalkableRepTilesWorld)
readWorld = fmap WalkableRepTilesWorld . W.readWorld '.' ['S'] . addRocksToRightAndTop

showWorld :: Int -> WalkableRepTilesWorld -> String
showWorld height world = undefined -- W.showWorld height charOrder world

printWorld :: Int -> WalkableRepTilesWorld -> IO ()
printWorld height world = undefined -- putStrLn $ WalkableBoundedWorld.showWorld height world

removeForbidden :: WalkableRepTilesWorld -> WalkableRepTilesWorld
removeForbidden w = undefined -- cutLayerWithLayer 'O' '#' w

progressByAStep :: WalkableRepTilesWorld -> WalkableRepTilesWorld
progressByAStep w = undefined -- removeForbidden $ combineWorlds $ map (\dir -> moveLayerInWorld 'O' dir w) allDirs

setOAtS = undefined -- fromJust . insertLayerAtPoint 'O' 'S'