#!/usr/bin/env stack
-- stack --resolver lts-22.0 ghci --package containers-0.6.7 --package linear-1.22 --package split-0.2.4 --package parallel-3.2.2.0 --package array-0.5.5.0 --package search-algorithms-0.3.2 --package unordered-containers-0.2.19.1 --package hashable-1.4.3.0

------------------------------------
------------------------------------
----  Day 17:  Clumsy Crucible  ----
------------------------------------
------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-22.0 ghc --package containers-0.6.7 --package linear-1.22 --package split-0.2.4 --package parallel-3.2.2.0 --package array-0.5.5.0 --package search-algorithms-0.3.2 --package unordered-containers-0.2.19.1 --package hashable-1.4.3.0 -- -threaded -O2 '.\day17.hs'
        
    For multi-core, use the following (replacing N4 with desired number of cores):
        ./day17 +RTS -N4
-}

------------
-- Output --
------------
-- *Main> day17part1
-- 936

-- *Main> day17part2
-- 1157


-------------
-- Imports --
-------------
-- import Data.List (
    -- foldl', nub, sort, transpose, isPrefixOf, findIndex, findIndices,
    -- intercalate, reverse, partition, intersperse, scanl', deleteBy)
import Data.List.Split (splitOn, chunksOf)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybeToList, catMaybes)
import Linear hiding (trace)
import qualified Linear
import Linear.V2
import Control.Monad (guard)
import Debug.Trace (trace)
import Control.Parallel.Strategies
import Data.Bits
import Data.Char
import qualified Data.Set as S
import Data.HashSet as H
import Algorithm.Search
import Data.List as L (sort, intersperse, foldl', findIndex, map, delete, null, concatMap, minimumBy, filter, transpose)
import Data.Maybe (fromJust, isJust)
import Data.Array as A (Array, (!), listArray)
import Data.Char (ord)
import Control.Monad (guard)
import Data.Ix
import Debug.Trace
import Data.Hashable


-------------
-- Program --
-------------
main = day17part2

data CrucibleState = CrucibleState {
    getLoc :: V2 Int,
    getDir :: V2 Int,
    getStraightCount :: Int
} deriving (Show, Ord, Eq)

instance Hashable CrucibleState where
    hashWithSalt s CrucibleState {getLoc = loc, getDir = dir, getStraightCount = count} =
        s `hashWithSalt` loc `hashWithSalt` dir `hashWithSalt` count

up = V2   0  (-1)
dn = V2   0    1
lt = V2 (-1)   0
rt = V2   1    0

allDirs = [up, dn, lt, rt]

inBounds :: V2 Int -> V2 Int -> Bool
inBounds (V2 width height) (V2 x y) = x >= 0 && y >= 0 && x < width && y < height

taxicabDist (V2 y1 x1) (V2 y2 x2) = abs (y2-y1) + abs (x2-x1)

day17part1 = do
    contents <- readFile "day17 (data).csv"
    let rows = lines contents
    let height = length rows
    let width = length $ head rows
    let dims = V2 width height
    
    let start = zero
    let end   = dims - V2 1 1
    
    let grid :: Array (V2 Int) Int
        grid = listArray ((V2 0 0), V2 (width - 1) (height - 1)) . L.map (read . (:[])) $ concat . L.transpose $ rows
    
    let graphFromGrid (CrucibleState pos dir count) = H.fromList $ do
            let posSteps
                 | count == 3 = L.filter (/= dir) . L.filter (/=(-dir)) $ allDirs
                 | otherwise  =                     L.filter (/=(-dir)) $ allDirs
            
            posStep <- posSteps
            
            let newCount
                 | posStep /= dir = 1
                 | otherwise      = count+1
            
            let newPos = pos + posStep
            
            guard $ inBounds dims newPos
            
            return (CrucibleState newPos posStep newCount)
    
    let costs (CrucibleState pos1 _ _) (CrucibleState pos2 _ _) = grid A.! pos2
    let heuristic (CrucibleState pos _ _) = taxicabDist pos end
    let goal (CrucibleState pos _ _) = pos == end
    
    let (Just (minCost,minPath)) = aStar graphFromGrid costs heuristic goal (CrucibleState start rt 1)
    
    -- mapM_ print minPath
    print minCost
    -- print (length minPath)

day17part2 = do
    contents <- readFile "day17 (data).csv"
    let rows = lines contents
    let height = length rows
    let width = length $ head rows
    let dims = V2 width height
    
    let start = zero
    let end   = dims - V2 1 1
    
    let grid :: Array (V2 Int) Int
        grid = listArray ((V2 0 0), V2 (width - 1) (height - 1)) . L.map (read . (:[])) $ concat . L.transpose $ rows
    
    let graphFromGrid (CrucibleState pos dir count) = H.fromList $ do
            let posSteps
                 | count < 4 = [dir]
                 | count >= 10 = L.filter (/= dir) . L.filter (/=(-dir)) $ allDirs
                 | otherwise  =                     L.filter (/=(-dir)) $ allDirs
            
            posStep <- posSteps
            
            let newCount
                 | posStep /= dir = 1
                 | otherwise      = count+1
            
            let newPos = pos + posStep
            
            guard $ inBounds dims newPos
            
            return (CrucibleState newPos posStep newCount)
    
    let costs (CrucibleState pos1 _ _) (CrucibleState pos2 _ _) = grid A.! pos2
    let heuristic (CrucibleState pos _ _) = taxicabDist pos end
    let goal (CrucibleState pos _ _) = pos == end
    
    let (Just (minCost,minPath)) = aStar graphFromGrid costs heuristic goal (CrucibleState start rt 1)
    
    -- mapM_ print minPath
    print minCost
    -- print (length minPath)