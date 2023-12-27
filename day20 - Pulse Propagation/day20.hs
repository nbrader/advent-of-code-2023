#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22 --package split-0.2.3.5 --package deque-0.4.4.1

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
import Deque.Strict as D


-------------
-- Program --
-------------
main = day20part1

-- START ModuleSpec Parse --
data ModuleType = FlipFlopType | ConjuctionType | BroadcasterType deriving (Show, Eq)
data ModuleSpec = ModuleSpec {moduleType :: ModuleType, moduleName :: String, moduleDestNames :: [String]} deriving (Show, Eq)

readModuleType :: String -> ModuleType
readModuleType "%" = FlipFlopType
readModuleType "&" = ConjuctionType
readModuleType _   = BroadcasterType

readModuleSpec :: String -> ModuleSpec
readModuleSpec inStr = ModuleSpec moduleType name destNames
  where (typeStr, after1) = splitAt 1 $ inStr
        [nameStr, destsStr] = splitOn " -> " $ after1
        destNames = splitOn ", " destsStr
        
        moduleType = readModuleType typeStr
        name
            | moduleType == BroadcasterType = "broadcaster"
            | otherwise                 = nameStr
-- END ModuleSpec Parse --

-- START System Parse --
data Pulse = Pulse {pulseRecipient :: String, isHigh :: Bool} deriving (Show, Eq)

data FlipFlop = FlipFlop {ffIsOn :: Bool} deriving (Show, Eq)
data Conjuction = Conjuction {cnLastPulses :: M.Map String Pulse} deriving (Show, Eq)
data Broadcaster = Broadcaster {bcPresses :: Int} deriving (Show, Eq)
data Module = FlipFlopModule FlipFlop | ConjuctionModule Conjuction | BroadcasterModule Broadcaster deriving (Show, Eq)
data ModuleAndRecipients = ModuleAndRecipients {marModule :: Module, marRecipients :: [String]} deriving (Show, Eq)

data System = System {sysModules :: M.Map String ModuleAndRecipients, sysPulses :: D.Deque Pulse} deriving (Show, Eq)
emptySystem = System mempty mempty

-- readSystem :: String -> System
readSystem inStr = foldl' updateSystemWithModuleSpec emptySystem modulesSpecs
  where modulesSpecs = map readModuleSpec . lines $ inStr

updateSystemWithModuleSpec :: System -> ModuleSpec -> System
updateSystemWithModuleSpec (System modules pulses) (ModuleSpec moduleType name destNames)
    = undefined

-- END System Parse --


day20part1 = do
  contents <- readFile "day20 (example).csv"
  let system = readSystem $ contents
  print system
