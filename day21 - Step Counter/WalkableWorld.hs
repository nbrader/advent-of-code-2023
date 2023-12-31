#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package split-0.2.3.5 --package safe-0.3.19

module WalkableWorld where

import qualified World

class WalkableWorld w where
  readWorld :: String -> (Int, w)
    -- Assumes all rows have equal length
    
  showWorld :: Int -> w -> String
  removeForbidden :: w -> w
  progressByAStep :: w -> w
  setOAtS :: w -> w
  asWorld :: w -> World.World
  oCount :: w -> Integer

printWorld :: (WalkableWorld w) => Int -> w -> IO ()
printWorld height w = putStrLn $ showWorld height w