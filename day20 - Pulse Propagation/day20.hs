#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22 --package split-0.2.3.5

-------------------------------------
-------------------------------------
----  Day 20: Pulse Propagation  ----
-------------------------------------
-------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package linear-1.22 -- '.\day20.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day20part1
-- 

-- *Main> day20part2
-- 


-------------
-- Imports --
-------------
import Data.List (foldl', nub, sort, sortBy, groupBy, delete)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybeToList, catMaybes)
import Linear hiding (trace)
import Linear.V2
import Debug.Trace (trace)
import Data.Ord
import Data.Function
import Data.Tuple


-------------
-- Program --
-------------
main = day20part1

data ModuleType = FlipFlop | Conjuction | Broadcaster deriving (Show, Eq)
data Module = Module {moduleType :: ModuleType, moduleName :: String, moduleDestNames :: [String]} deriving (Show, Eq)

readModuleType :: String -> ModuleType
readModuleType "%" = FlipFlop
readModuleType "&" = Conjuction
readModuleType _   = Broadcaster

readModule :: String -> Module
readModule inStr = Module moduleType name destNames
  where (typeStr, after1) = splitAt 1 $ inStr
        [nameStr, destsStr] = splitOn " -> " $ after1
        destNames = splitOn ", " destsStr
        
        moduleType = readModuleType typeStr
        name
            | moduleType == Broadcaster = "broadcaster"
            | otherwise                 = nameStr

day20part1 = do
  contents <- readFile "day20 (example).csv"
  let modules = map readModule . lines $ contents
  mapM_ print modules
