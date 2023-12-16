#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22 --package split-0.2.3.5 --package parallel-3.2.2.0

-------------------------------------------
-------------------------------------------
----  Day 16:  The Floor Will Be Lava  ----
-------------------------------------------
-------------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package linear-1.22 -- -threaded -O2 '.\day16.hs'
-}

------------
-- Output --
------------
-- *Main> day16part1
-- 

-- *Main> day16part2
-- 


-------------
-- Imports --
-------------
import Data.List (
    foldl', nub, sort, transpose, isPrefixOf, findIndex, findIndices,
    intercalate, reverse, partition, intersperse, scanl', deleteBy)
import Data.List.Split (splitOn, chunksOf)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybeToList, catMaybes)
import Linear
import Linear.V2
import Control.Monad (guard)
import Debug.Trace (trace)
import Control.Parallel.Strategies
import Data.Bits
import Data.Char
import qualified Data.Set as S


-------------
-- Program --
-------------
main = day16part1

data LocDir = LocDir {
    getLoc :: V2 Int,
    getDir :: V2 Int } deriving (Show, Ord, Eq)

up = V2   0  (-1)
dn = V2   0    1
lt = V2 (-1)   0
rt = V2   1    0


data World = World {
    worldObjs :: M.Map (V2 Int) Char,
    worldDims :: V2 Int } deriving (Show, Eq)

emptyWorld dims = World mempty dims


data Beam = Beam {
        beamLocDirList :: [LocDir],
        beamLocDirSet :: S.Set LocDir } deriving (Show, Eq)
emptyBeam = Beam mempty mempty

inBounds :: V2 Int -> V2 Int -> Bool
inBounds (V2 width height) (V2 x y) = x >= 0 && y >= 0 && x < width && y < height

getNextLockDirsFromHitLocDir :: World -> LocDir -> [LocDir]
getNextLockDirsFromHitLocDir world@(World objs dims) (LocDir loc dir)
    = let c = M.lookup loc objs
      in filter (inBounds dims) $ case c of
            Just '|'  ->      if dir == up then [upLocDir]
                         else if dir == dn then [dnLocDir]
                         else                   [upLocDir, dnLocDir]
            Just '-'  ->      if dir == lt then [ltLocDir]
                         else if dir == rt then [rtLocDir]
                         else                   [ltLocDir, rtLocDir]
            Just '/'  ->      if dir == rt then [upLocDir]
                         else if dir == up then [rtLocDir]
                         else if dir == dn then [ltLocDir]
                         else if dir == lt then [dnLocDir]
                         else error "Invalid Hit Dir"
            Just '\\' ->      if dir == rt then [dnLocDir]
                         else if dir == dn then [rtLocDir]
                         else if dir == up then [ltLocDir]
                         else if dir == lt then [upLocDir]
                         else error "Invalid Hit Dir"
            Nothing -> []
  where upLocDir = LocDir (loc + up) up
        dnLocDir = LocDir (loc + dn) dn
        ltLocDir = LocDir (loc + lt) lt
        rtLocDir = LocDir (loc + rt) rt

getNextHit :: World -> LocDir -> LocDir
getNextHit world (LocDir loc dir) = -- To Do

progressBeamToNextHit :: World -> Beam -> Beam
progressBeamToNextHit world@(World objs dims) (Beam locDirList@((LocDir loc dir):_) locDirSet)
    = Beam (newLocDirs ++ locDirList) (S.union (S.fromList newLocDirs) locDirSet)
  where nextLocDirs = getNextLockDirsFromHitLocDir world (LocDir loc dir)
        
  
  
                        ltBeam = getFullResultingBeam world (Beam (ltLocDir:locDirList) (S.insert ltLocDir locDirSet))
                        rtBeam = getFullResultingBeam world (Beam (rtLocDir:locDirList) (S.insert rtLocDir locDirSet))
                    in beamLocDirList ltBeam ++ beamLocDirList rtBeam -- To Do

getFullResultingBeam :: World -> Beam -> Beam
getFullResultingBeam world initBeam = fst $ until end (progress world) (initBeam, emptyBeam)
  where end      (beam, prevBeam) = beam == prevBeam
        progress w (beam, prevBeam) = (progressBeamToNextHit w beam, beam)
        

readChar2Ds :: (Num a, Enum a) => [Char] -> [(V2 a, Char)]
readChar2Ds inStr = do
    let rows = lines inStr
    (y,row)  <- zip [0..] rows
    (x,char) <- zip [0..] row
    
    return (V2 x y, char)

readWorld :: String -> World
readWorld inStr = foldl' updateWorld (emptyWorld dims) char2Ds
  where rows = lines inStr
        height = length rows
        width = length $ head rows
        dims = V2 width height
        char2Ds = readChar2Ds inStr
        updateWorld (World objs dims) (loc, char)
            = case char of
                '.' -> World objs dims
                c   -> World (M.insert loc c objs) dims

day16part1 = do
    contents <- readFile "day16 (example).csv"
    let world = readWorld $ contents
    print world
    print $ getFullResultingBeam world $ Beam [start] (S.fromList [start])
  where start = LocDir (V2 0 0) (V2 1 0)