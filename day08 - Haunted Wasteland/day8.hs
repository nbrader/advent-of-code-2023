#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22 --package fastmemo-0.1.1

-------------------------------------
-------------------------------------
----  Day 8:  Haunted Wasteland  ----
-------------------------------------
-------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22 -- '.\day8.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day8part1
-- 

-- *Main> day8part2
-- 


-------------
-- Imports --
-------------
import Data.Char (isDigit, isSpace, isAlphaNum)
import Data.List (sort, tails, isPrefixOf, groupBy, find, nub, foldl', group, sort, intercalate)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (fromJust, fromMaybe)
import Data.Map as M hiding (map, filter, take, drop, foldl', null)
import Debug.Trace (trace)
import Data.Tuple (swap)
import Data.Function (on, fix)
import Data.Function.FastMemo (memoize)
import Data.Ord (comparing)


-------------
-- Program --
-------------
main = day8part2

data Paper = Paper {piDirs :: String, piNetwork :: M.Map String (String, String)} deriving (Show)

trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs

readPaper :: String -> Paper
readPaper inStr
    = Paper dirs network
  where (dirsStr, networkStr) = fmap trim $ break (=='\n') inStr
        
        dirs = dirsStr
        network = M.fromList . map ((\[source, targetsStr] -> (source, readTargets targetsStr)) . splitOn " = ") . lines $ networkStr
        
        readTargets = (\[l,r] -> (l,r)) . words . map (\c -> if isAlphaNum c then c else ' ')

unsafeLookup :: (Ord k) => k -> M.Map k v -> v
unsafeLookup x m = fromJust $ M.lookup x m

-- paperRunLength :: Paper -> Int
-- paperRunLength p = length . takeWhile (\loc -> loc /= "ZZZ") . scanr (\dir loc -> let (left, right) = unsafeLookup loc (piNetwork p) in case dir of {'L' -> left; 'R' -> right} ) "AAA" . repeatedDirsReversed $ p

paperRunLength :: Paper -> Int
paperRunLength p = (\(loc,dirs,count) -> count) . (\initDirs -> until (\(loc,dirs,count) -> {-trace loc $-} loc == "ZZZ") (\(loc,(dir:dirs),count) -> (let (left,right) = unsafeLookup loc (piNetwork p) in case dir of {'L' -> left; 'R' -> right},dirs,count+1)) ("AAA",initDirs,0)) . repeatedDirs $ p

paperRunLength2 :: Paper -> Int
paperRunLength2 p = (\(locs,dirs,count) -> count) . (\initDirs -> until (\(locs,dirs,count) -> {-trace (intercalate "\n" locs ++ "\n") $-} all (`elem` ends) locs) (\(locs,(dir:dirs),count) -> (let leftAndrights = map (\loc -> unsafeLookup loc (piNetwork p)) locs in map (\(left,right) -> case dir of {'L' -> left; 'R' -> right}) leftAndrights,dirs,count+1)) (starts ,initDirs,0)) . repeatedDirs $ p
  where starts = nodesThatEndWithA p
        ends   = nodesThatEndWithZ p

nodesThatEndWithA :: Paper -> [String]
nodesThatEndWithA = filter ((=='A') . last) . M.keys . piNetwork

nodesThatEndWithZ :: Paper -> [String]
nodesThatEndWithZ = filter ((=='Z') . last) . M.keys . piNetwork

repeatedDirs :: Paper -> String
repeatedDirs p = piDirs p ++ repeatedDirs p

repeatedDirsReversed :: Paper -> String
repeatedDirsReversed p = reverse (piDirs p) ++ repeatedDirsReversed p

day8part1 = do
  contents <- readFile "day8 (data).csv"
  print . paperRunLength . readPaper $ contents

day8part2 = do
  contents <- readFile "day8 (data).csv"
  print . paperRunLength2 . readPaper $ contents