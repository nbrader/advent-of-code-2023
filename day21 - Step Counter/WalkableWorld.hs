#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci

module WalkableWorld where

import World

class WalkableWorld w where
  readWorld :: String -> (Int, w)
    -- Assumes all rows have equal length
    
  showWorld :: Int -> w -> String
  printWorld :: Int -> w -> IO ()
  removeForbidden :: w -> w
  progressByAStep :: w -> w
  setOAtS :: w -> w
  coreWorld :: w -> World