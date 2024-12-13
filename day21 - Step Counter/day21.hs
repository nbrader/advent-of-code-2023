#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3

--------------------------------
--------------------------------
----  Day 21: Step Counter  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3 -- '.\day21.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day21part1
-- 3740

-- *Main> day21part2
-- 


-------------
-- Imports --
-------------
import Util (iterate')

import WalkableWorld
-- import WalkableBoundedWorld
import WalkableBoundedWorldOptimised
-- import WalkableRepTilesWorld
import WalkableRepTilesWorldOptimised

-------------
-- Program --
-------------
main = day21part2

-- Main
day21part1 = do
    contents <- readFile "day21 (data).csv"
    let (height, world) = (readWorld :: String -> (Int,WalkableBoundedWorldOptimised)) contents
    let worldBeforeStep = setOAtS world
    let futureWorlds = iterate progressByAStep worldBeforeStep
    -- print world
    -- mapM_ (printWorld height) (take 7 futureWorlds)
    print . oCount . (!!64) $ futureWorlds

duplicateWorldNxNAsString :: Int -> String -> String
duplicateWorldNxNAsString n inStr = unlines . concat . replicate n . map (concat . replicate n) . lines $ inStr


-- TO DO: Use QuadTree to speed up search
day21part2 = do
    contents <- readFile "day21 (data).csv"
    let (height, world) = (readWorld :: String -> (Int,WalkableRepTilesWorldOptimised)) contents
    let worldBeforeStep = setOAtS world
    let futureWorlds = iterate' progressByAStep worldBeforeStep
    -- print world
    -- mapM_ (printWorld height) (take 26501365 futureWorlds)
    print . oCount . (!!26501365) $ futureWorlds
