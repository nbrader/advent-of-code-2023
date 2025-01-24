#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3 --package ansi-terminal-0.11.5

--------------------------------
--------------------------------
----  Day 21: Step Counter  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 --package safe-0.3.19 --package QuickCheck-2.14.3 --package ansi-terminal-0.11.5 -- '.\day21 (animating).hs' -O2
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
import WalkableBoundedWorld
import Control.Concurrent (threadDelay)
import System.Console.ANSI (clearScreen, setCursorPosition)

main = day21part1

day21part1 = do
    contents <- readFile "day21 (data).csv"
    let (height, world) = (readWorld :: String -> (Int, WalkableBoundedWorld)) contents
    let worldBeforeStep = setOAtS world
    let futureWorlds = iterate progressByAStep worldBeforeStep
    
    animateFrames 3 height futureWorlds

animateFrames :: Int -> Int -> [WalkableBoundedWorld] -> IO ()
animateFrames frameRate height worlds = mapM_ (animateStep frameRate height) (take 100 worlds)

animateStep :: Int -> Int -> WalkableBoundedWorld -> IO ()
animateStep frameRate height world = do
    clearScreen  -- Clear the console
    setCursorPosition 0 0  -- Move cursor to top-left
    printWorld 132 world  -- Print the current state
    threadDelay (1000000 `div` frameRate)  -- Control frame rate