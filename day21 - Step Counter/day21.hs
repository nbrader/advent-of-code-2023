#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19

--------------------------------
--------------------------------
----  Day 21: Step Counter  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 -- '.\day21.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day21part1
-- 

-- *Main> day21part2
-- 


-------------
-- Imports --
-------------
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Bits

import Util (iterate')
import Layer (allDirs)
import World as W ( World(..)
                  , emptyWorld
                  , combineTwoWorlds
                  , combineWorlds
                  , hasPoint
                  , moveLayerInWorld
                  , movePointInWorld
                  , cutLayerWithLayer
                  , setPoint
                  , insertLayerAtPoint
                  , isOverlappingLayers )

import WalkableWorld
import WalkableBoundedWorld
import WalkableRepTilesWorld

-------------
-- Program --
-------------
main = day21part2

-- Main
day21part1 = do
    contents <- readFile "day21 (data).csv"
    let (height, world) = (readWorld :: String -> (Int,WalkableBoundedWorld)) contents
    let worldBeforeStep = setOAtS world
    let futureWorlds = iterate progressByAStep worldBeforeStep
    -- print world
    -- mapM_ (printWorld 12) (take 7 futureWorlds)
    print . oCount . (!!64) $ futureWorlds

duplicateWorldNxN :: Int -> String -> String
duplicateWorldNxN n inStr = unlines . concat . replicate n . map (concat . replicate n) . lines $ inStr

day21part2 = do
    contents <- readFile "day21 (example).csv"
    let (originalHeight, originalWorld) = (readWorld :: String -> (Int,WalkableBoundedWorld)) contents
    let dupeCount = 2*((26501365 `div` originalHeight) + 1)
    let semiDupeCount = dupeCount `div` 2
    let originalWidth = worldWidth (asWorld originalWorld)
    let (height, world') = (readWorld :: String -> (Int,WalkableBoundedWorld)) (duplicateWorldNxN dupeCount contents)
    let world = WalkableBoundedWorld $ movePointInWorld 'S' (semiDupeCount*originalWidth,-semiDupeCount*originalHeight) (asWorld world')
    let worldBeforeStep = setOAtS world
    let futureWorlds = iterate' progressByAStep worldBeforeStep
    -- print world
    -- mapM_ (B.printWorld 150) (take 7 futureWorlds)
    print . oCount . (!!26501365) $ futureWorlds
