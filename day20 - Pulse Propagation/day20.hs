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
data ModuleSpec = ModuleSpec {moduleType :: ModuleType, moduleName :: String, moduleRecipients :: [String]} deriving (Show, Eq)

readModuleType :: String -> ModuleType
readModuleType "%" = FlipFlopType
readModuleType "&" = ConjuctionType
readModuleType _   = BroadcasterType

readModuleSpec :: String -> ModuleSpec
readModuleSpec inStr = ModuleSpec moduleType name recipients
  where (typeStr, after1) = splitAt 1 $ inStr
        [nameStr, destsStr] = splitOn " -> " $ after1
        recipients = splitOn ", " destsStr
        
        moduleType = readModuleType typeStr
        name
            | moduleType == BroadcasterType = "broadcaster"
            | otherwise                 = nameStr
-- END ModuleSpec Parse --

-- START System Parse --
data Pulse = Pulse {pulseRecipient :: String, isHigh :: Bool} deriving (Show, Eq)

data FlipFlop = FlipFlop {ffIsOn :: Bool} deriving (Show, Eq)
data Conjuction = Conjuction {cnLastPulseIsHighMap :: M.Map String Bool} deriving (Show, Eq)
data Broadcaster = Broadcaster {bcPresses :: Int} deriving (Show, Eq)
data Module = FlipFlopModule FlipFlop | ConjuctionModule Conjuction | BroadcasterModule Broadcaster deriving (Show, Eq)
data ModuleAndRecipients = ModuleAndRecipients {marModule :: Module, marRecipients :: [String]} deriving (Show, Eq)

data System = System {sysModulesAndRecipients :: M.Map String ModuleAndRecipients, sysPulses :: D.Deque Pulse} deriving (Show, Eq)
emptySystem = System mempty mempty

-- readSystem :: String -> System
readSystem inStr = foldl' updateSystemWithModuleSpec emptySystem modulesSpecs
  where modulesSpecs = map readModuleSpec . lines $ inStr

updateSystemWithModuleSpec :: System -> ModuleSpec -> System
updateSystemWithModuleSpec s m
    = s {sysModulesAndRecipients = updateModulesAndRecipientsWithModule (sysModulesAndRecipients s)}
  where updateModulesAndRecipientsWithModule :: M.Map String ModuleAndRecipients -> M.Map String ModuleAndRecipients
        updateModulesAndRecipientsWithModule oldModulesAndRecipients = M.insert newModuleName (ModuleAndRecipients newModule (moduleRecipients m)) newModulesAndRecipients
          where newModuleName = moduleName m
                recipients = moduleRecipients m
                
                -- find names of all existing senders in modules and populate map with low pulse
                existingSenders = M.map (const False) $ M.filter (\oldModuleAndRecipient -> newModuleName `elem` marRecipients oldModuleAndRecipient) oldModulesAndRecipients
                
                newModule = case moduleType m of
                    FlipFlopType -> FlipFlopModule (FlipFlop False)
                    ConjuctionType -> ConjuctionModule (Conjuction existingSenders)
                    BroadcasterType -> BroadcasterModule (Broadcaster 0)
                
                --update all existing ConjuctionType recipients
                newModulesAndRecipients = M.mapWithKey updateIfConjunctionModule oldModulesAndRecipients
                
                updateIfConjunctionModule :: String -> ModuleAndRecipients -> ModuleAndRecipients
                updateIfConjunctionModule recipientName m'@(ModuleAndRecipients (ConjuctionModule (Conjuction lastPulseIsHighMap)) _)
                    | recipientName `elem` recipients = m' {marModule = ConjuctionModule (Conjuction (M.insert newModuleName False lastPulseIsHighMap))}
                    | otherwise                       = m'
                updateIfConjunctionModule k m' = m'

setPresses :: Int -> System -> System
setPresses newPresses system = system { sysModulesAndRecipients = updatedModulesAndRecipients }
  where
    updatedModulesAndRecipients = M.map updateIfBroadcaster $ sysModulesAndRecipients system
    
    updateIfBroadcaster :: ModuleAndRecipients -> ModuleAndRecipients
    updateIfBroadcaster mar@(ModuleAndRecipients (BroadcasterModule _) recips) = 
      mar { marModule = BroadcasterModule (Broadcaster newPresses) }
    updateIfBroadcaster mar = mar

day20part1 = do
    contents <- readFile "day20 (example 2).csv"
    let system = setPresses 10 $ readSystem $ contents
    mapM_ print . M.keys $ sysModulesAndRecipients system
    putStrLn ""
    mapM_ print . M.elems $ sysModulesAndRecipients system
