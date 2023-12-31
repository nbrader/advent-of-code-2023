#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22 --package split-0.2.3.5 --package deque-0.4.4.1 --package safe-0.3.19

module Layer where

-------------
-- Imports --
-------------
import Data.List (foldl', nub, sort, sortBy, groupBy, delete, find, transpose, findIndex)
import Data.List.Split (splitOn, chunksOf)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybeToList, catMaybes, fromMaybe)
import Linear hiding (trace, transpose)
import Linear.V2
import Debug.Trace (trace)
import Data.Ord
import Data.Function
import Data.Tuple
import GHC.Exts as F
import Data.Bits
import Control.Monad (guard, join)
import Data.Monoid
import Data.Foldable
import Data.Ord
import Safe (atMay)

import Util (replace, iterate')


-- Each obj has a shape encoded as bits of an Integer.
type SingularPoint = (Int,Int)
type Layer = Integer

-- Converts a 2D point to a 1D index
pointToIndex :: Int -> SingularPoint -> Int
pointToIndex width (x, y) = y * width + x

pointToLayer :: Int -> SingularPoint -> Layer
pointToLayer width (x,y) = moveLayer width (x,y) 1

moveLayer :: Int -> (Int,Int) -> Layer -> Layer
moveLayer width (dx,dy) pts = pts `shift` pointToIndex width (dx,dy)

movePoint :: Int -> (Int,Int) -> (Int,Int) -> (Int,Int)
movePoint width (dx,dy) (x,y) = (x+dx,y+dy)


-- isOverlapping n n
-- time:  O(n) (approx.) [0 < n < 10000000000, 0 < secs < 7.97]
-- space: O(n) (approx.) [0 < n < 10000000000, 0 < bytes < 30000141704]
-- https://nux-pc/svn/Nux-SVN/My Programming/Scratchings/Advent of Code 2022/day17 (bits).hs/?r=1569
isOverlapping :: Layer -> Layer -> Bool
isOverlapping ps1 ps2 = (ps1 .&. ps2) /= zeroBits

diff :: Layer -> Layer -> Layer
diff ps1 ps2 = (ps1 .&. complement ps2)

up, dn, lt, rt :: (Integral a) => (a,a)
up = (  0 ,   1 )
dn = (  0 , (-1))
lt = ((-1),   0 )
rt = (  1 ,   0 )

allDirs :: (Integral a) => [(a,a)]
allDirs = [up,dn,lt,rt]
