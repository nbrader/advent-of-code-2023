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
import Linear ((*^))
import qualified Linear
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
    worldRows :: [String],
    worldObjs :: M.Map (V2 Int) Char,
    worldDims :: V2 Int } deriving (Show, Eq)

emptyWorld strs dims = World strs mempty dims


data Beam = Beam {
        beamLocDirList :: [LocDir],
        beamLocDirSet :: S.Set LocDir } deriving (Show, Eq)
emptyBeam = Beam mempty mempty


inBounds :: V2 Int -> V2 Int -> Bool
inBounds (V2 width height) (V2 x y) = x >= 0 && y >= 0 && x < width && y < height

inWorld :: World -> V2 Int -> Bool
inWorld w v = inBounds (worldDims w) v

getNextLocDirsFromHitLocDir :: World -> LocDir -> [LocDir]
getNextLocDirsFromHitLocDir world@(World rows objs dims) (LocDir loc dir)
    = let c = M.lookup loc objs
      in traceShow "getNextLocDirsFromHitLocDir (with filter):" $ filter (inWorld world . getLoc) $ traceShow "getNextLocDirsFromHitLocDir (without filter):" $ case c of
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

getNextLocDirsFromHitLocDirAndChar :: World -> LocDir -> Char -> [LocDir]
getNextLocDirsFromHitLocDirAndChar world@(World rows objs dims) (LocDir loc dir) char
    = case char of
            '|'  ->      if dir == up then [upLocDir]
                         else if dir == dn then [dnLocDir]
                         else                   [upLocDir, dnLocDir]
            '-'  ->      if dir == lt then [ltLocDir]
                         else if dir == rt then [rtLocDir]
                         else                   [ltLocDir, rtLocDir]
            '/'  ->      if dir == rt then [upLocDir]
                         else if dir == up then [rtLocDir]
                         else if dir == dn then [ltLocDir]
                         else if dir == lt then [dnLocDir]
                         else error "Invalid Hit Dir"
            '\\' ->      if dir == rt then [dnLocDir]
                         else if dir == dn then [rtLocDir]
                         else if dir == up then [ltLocDir]
                         else if dir == lt then [upLocDir]
                         else error "Invalid Hit Dir"
  where upLocDir = LocDir (loc + up) up
        dnLocDir = LocDir (loc + dn) dn
        ltLocDir = LocDir (loc + lt) lt
        rtLocDir = LocDir (loc + rt) rt

getFullWorldSliceInDirOrder :: World -> LocDir -> String
getFullWorldSliceInDirOrder world (LocDir (V2 x y) dir)
    | dir == up = reverse . (!! x) . transpose $ rows
    | dir == dn =           (!! x) . transpose $ rows
    | dir == lt = reverse . (!! y)             $ rows
    | dir == rt =           (!! y)             $ rows
    | otherwise = error "Invalid Dir"
  where rows = worldRows world

getForwardWorldSliceInDirOrder :: World -> LocDir -> String
getForwardWorldSliceInDirOrder world (LocDir (V2 x y) dir)
    | dir == dn = drop y fullSlice
    | dir == rt = drop x fullSlice
    | dir == up = drop (height-1-y) fullSlice
    | dir == lt = drop (width -1-x) fullSlice
    | otherwise = error "Invalid Dir"
  where fullSlice = getFullWorldSliceInDirOrder world (LocDir (V2 x y) dir)
        (V2 width height) = worldDims world

getUpToNextHit :: World -> LocDir -> ([LocDir], Maybe Char)
getUpToNextHit world locDir@(LocDir loc dir)
    | hitsObjBeforeWall = traceShow "getUpToNextHit hitsObjBeforeWall:" $ (forwardWorldSliceLocDirs, Just hit)
    | otherwise         = traceShow "getUpToNextHit otherwise:" $ (forwardWorldSliceLocDirs, Nothing)
  where forwardWorldSlice = getForwardWorldSliceInDirOrder world locDir
        hitsObjBeforeWall = any (`elem` "|-/\\") forwardWorldSlice
        (before,hit:after) = break (`elem` "|-/\\") forwardWorldSlice
        sliceUpToHitLength
            | hitsObjBeforeWall = length before
            | otherwise         = length forwardWorldSlice - 1
        forwardWorldSliceLocDirs = [LocDir (loc + i *^ dir) dir | i <- [sliceUpToHitLength,(sliceUpToHitLength-1)..1]]

getUpToNextHitInclusive :: World -> LocDir -> ([LocDir], Maybe Char)
getUpToNextHitInclusive world locDir@(LocDir loc dir)
    | hitsObjBeforeWall = traceShow "getUpToNextHit hitsObjBeforeWall:" $ (locDir:forwardWorldSliceLocDirs, Just hit)
    | otherwise         = traceShow "getUpToNextHit otherwise:" $ (locDir:forwardWorldSliceLocDirs, Nothing)
  where forwardWorldSlice = getForwardWorldSliceInDirOrder world locDir
        hitsObjBeforeWall = any (`elem` "|-/\\") forwardWorldSlice
        (before,hit:after) = break (`elem` "|-/\\") forwardWorldSlice
        sliceUpToHitLength
            | hitsObjBeforeWall = length before
            | otherwise         = length forwardWorldSlice - 1
        forwardWorldSliceLocDirs = [LocDir (loc + i *^ dir) dir | i <- [sliceUpToHitLength,(sliceUpToHitLength-1)..1]]

combineBeams :: World -> [Beam] -> Beam
combineBeams world beams = Beam (concat (map beamLocDirList beams)) (S.unions (map beamLocDirSet beams))

traceShow msg x' = let x = x' in trace (msg ++ "\n" ++ show x ++ "\n") x

readChar2Ds :: (Num a, Enum a) => [Char] -> [(V2 a, Char)]
readChar2Ds inStr = do
    let rows = lines inStr
    (y,row)  <- zip [0..] rows
    (x,char) <- zip [0..] row
    
    return (V2 x y, char)

readWorld :: String -> World
readWorld inStr = foldl' updateWorld (World rows mempty dims) char2Ds
  where rows = lines inStr
        height = length rows
        width = length $ head rows
        dims = V2 width height
        char2Ds = readChar2Ds inStr
        updateWorld (World rs objs dims) (loc, char)
            = case char of
                '.' -> World rs objs dims
                c   -> World rs (M.insert loc c objs) dims


--------- Unverified functions below this line -----------


getAllBeamlines :: World -> [LocDir] -> ([BeamLine], S.Set LocDir)
getAllBeamlines world initEmissions = (\([], finished, visited) -> (finished, visited)) $ until end (progress world) (initEmissions, mempty, mempty)
  where end        (emissions, finished, visited) = null emissions
        
        progress :: World -> ([LocDir],[BeamLine],S.Set LocDir) -> ([LocDir],[BeamLine],S.Set LocDir)
        progress w (emissions, prevBeamLines, visited) = let childBeamLineInfos = map (getChildBeamLineInfo w visited) emissions
                                                             childEmissions   = map (\(emissions',          _,        _) -> emissions') childBeamLineInfos
                                                             childBeamLines   = map (\(         _, beamLines',        _) -> beamLines') childBeamLineInfos
                                                             childVisitedSets = map (\(         _,          _, visited') -> visited'  ) childBeamLineInfos
                                                         in (concat childEmissions, prevBeamLines ++ filter (not . null) childBeamLines, S.union visited (S.unions childVisitedSets))

type BeamLine = [LocDir]

getChildBeamLineInfo :: World -> S.Set LocDir -> LocDir -> ([LocDir], BeamLine, S.Set LocDir)
getChildBeamLineInfo world@(World rows objs dims) visited emission@(LocDir emissionloc emissionDir) = allPaths
  where (newBeamLine, maybeHitChar) = getUpToNextHitInclusive world (LocDir emissionloc emissionDir)
        allPaths = if head newBeamLine `S.member` visited
                    then ([], dropWhile (`S.member` visited) newBeamLine, S.fromList newBeamLine `S.union` visited)
                    else case maybeHitChar of
                                Nothing      -> ([], newBeamLine, S.fromList newBeamLine `S.union` visited)
                                Just hitChar -> let hitLocDir = head newBeamLine
                                                    nextEmissions = filter (not . (`S.member` visited)) $ getNextLocDirsFromHitLocDirAndChar world hitLocDir hitChar
                                                in (nextEmissions, newBeamLine, S.fromList newBeamLine `S.union` visited)

getFullResultingBeam' :: World -> Beam -> Beam
getFullResultingBeam' world beam@(Beam [] _) = error ("progressBeamToNextHit world beam@(Beam [] _): " ++ show (world, beam))
getFullResultingBeam' world@(World rows objs dims) oldBeam@(Beam locDirList@((LocDir loc dir):_) locDirSet)
    = trace ("progressBeamToNextHit locDirList:\n" ++ show locDirList ++ "\n") $ combineBeams world allBeams
  where (sliceUpToNextHit, maybeHitChar) = getUpToNextHit world (LocDir loc dir)
        maybeIndexOfPrevVisit = findIndex (`S.member` locDirSet) sliceUpToNextHit
        allBeams = case trace ("maybeIndexOfPrevVisit: " ++ show (maybeIndexOfPrevVisit, locDirSet, sliceUpToNextHit)) maybeIndexOfPrevVisit of
            Just indexOfPrevVisit -> let sliceUpToPrevVisit = take (indexOfPrevVisit-1) sliceUpToNextHit
                                     in [Beam (sliceUpToPrevVisit ++ locDirList) (S.union (S.fromList sliceUpToPrevVisit) locDirSet)]
            Nothing -> let newBeam = Beam (sliceUpToNextHit ++ locDirList) (S.union (S.fromList sliceUpToNextHit) locDirSet)
                       in case maybeHitChar of
                            Nothing      -> [newBeam]
                            Just hitChar -> let hitLocDir = case sliceUpToNextHit of
                                                    []             -> (LocDir loc dir)
                                                    (hitInSlice:_) -> hitInSlice
                                                nextLocDirsFromHitLocDir = filter (not . (`S.member` locDirSet)) $ getNextLocDirsFromHitLocDir world hitLocDir
                                                nextBeams
                                                    | null nextLocDirsFromHitLocDir = [oldBeam]
                                                    | otherwise = map (\nextLocDir -> Beam (nextLocDir : beamLocDirList newBeam) (S.insert nextLocDir (beamLocDirSet newBeam))) nextLocDirsFromHitLocDir
                                            in map (getFullResultingBeam' world) nextBeams

day16part1 = do
    contents <- readFile "day16 (example).csv"
    let world = readWorld $ contents
    let (beamLines, visited) = getAllBeamlines world [start]
    mapM_ print . S.toList $ visited
  where start = LocDir (V2 0 0) (V2 1 0)