#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22 --package split-0.2.3.5 --package deque-0.4.4.1 --package safe-0.3.19

--------------------------------
--------------------------------
----  Day 21: Step Counter  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package linear-1.22 --package deque-0.4.4.1 --package safe-0.3.19 -- '.\day21.hs' -O2
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
import Layer (pointToIndex, pointToLayer, moveLayer, movePoint, isOverlapping, diff, up, dn, lt, rt, allDirs)
import World (SingularPoint, Layer, World(..), emptyWorld, readWorld, showWorld, printWorld, combineTwoWorlds, combineWorlds, hasPoint, moveLayerInWorld, movePointInWorld, cutLayerWithLayer, setPoint, insertLayerAtPoint, isOverlappingLayers)


-------------
-- Program --
-------------
main = day21part2


charOrder :: Char -> Char -> Ordering
charOrder c1 c2 = comparing specialRank c1 c2 <> compare c1 c2
  where compareSpecial = comparing specialRank
        
        specialRank c = findIndex (==c) ['O','S','#','.']

addRocksToRightAndTop :: String -> String
addRocksToRightAndTop inStr = unlines . (\rows -> map (const '#') (head rows) : rows) . map (++"#") . lines $ inStr

removeForbidden :: World -> World
removeForbidden w = cutLayerWithLayer 'O' '#' w

progressByAStep :: World -> World
progressByAStep w = removeForbidden $ combineWorlds $ map (\dir -> moveLayerInWorld 'O' dir w) allDirs

-- Main
day21part1 = do
    contents <- readFile "day21 (data).csv"
    let (height, world) = readWorld '.' ['S'] (addRocksToRightAndTop contents)
    let worldBeforeStep = fromJust $ insertLayerAtPoint 'O' 'S' world
    let futureWorlds = iterate progressByAStep worldBeforeStep
    -- print width
    -- print world
    -- mapM_ (printWorld 12 charOrder) (take 7 futureWorlds)
    print . popCount . fromJust . M.lookup 'O' . worldLayers . (!!64) $ futureWorlds

duplicateWorldNxN :: Int -> String -> String
duplicateWorldNxN n inStr = unlines . concat . replicate n . map (concat . replicate n) . lines $ inStr

day21part2 = do
    contents <- readFile "day21 (example).csv"
    let (originalHeight, originalWorld) = readWorld '.' ['S'] contents
    let dupeCount = 2*((26501365 `div` originalHeight) + 1)
    let semiDupeCount = dupeCount `div` 2
    let originalWidth = worldWidth originalWorld
    let (height, world') = readWorld '.' ['S'] (duplicateWorldNxN dupeCount contents)
    let world = movePointInWorld 'S' (semiDupeCount*originalWidth,-semiDupeCount*originalHeight) world'
    let worldBeforeStep = fromJust $ insertLayerAtPoint 'O' 'S' world
    let futureWorlds = iterate' progressByAStep worldBeforeStep
    -- print width
    -- print world
    -- mapM_ (printWorld 150 charOrder) (take 7 futureWorlds)
    print . popCount . fromJust . M.lookup 'O' . worldLayers . (!!26501365) $ futureWorlds
