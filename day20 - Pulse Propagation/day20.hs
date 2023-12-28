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
import Data.List (foldl', nub, sort, sortBy, groupBy, delete, find)
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
data ModuleAndRecipients = ModuleAndRecipients {marName :: String, marModule :: Module, marRecipients :: [String]} deriving (Show, Eq)

data System = System {sysModulesAndRecipients :: M.Map String ModuleAndRecipients, sysPulses :: D.Deque Pulse, sysProcessedPulses :: [Pulse]} deriving (Show, Eq)
emptySystem = System mempty mempty mempty

-- readSystem :: String -> System
readSystem inStr = foldl' updateSystemWithModuleSpec emptySystem modulesSpecs
  where modulesSpecs = map readModuleSpec . lines $ inStr

updateSystemWithModuleSpec :: System -> ModuleSpec -> System
updateSystemWithModuleSpec s m
    = s {sysModulesAndRecipients = updateModulesAndRecipientsWithModule (sysModulesAndRecipients s)}
  where updateModulesAndRecipientsWithModule :: M.Map String ModuleAndRecipients -> M.Map String ModuleAndRecipients
        updateModulesAndRecipientsWithModule oldModulesAndRecipients = M.insert newModuleName (ModuleAndRecipients newModuleName newModule (moduleRecipients m)) newModulesAndRecipients
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
                updateIfConjunctionModule recipientName m'@(ModuleAndRecipients _ (ConjuctionModule (Conjuction lastPulseIsHighMap)) _)
                    | recipientName `elem` recipients = m' {marModule = ConjuctionModule (Conjuction (M.insert newModuleName False lastPulseIsHighMap))}
                    | otherwise                       = m'
                updateIfConjunctionModule k m' = m'

setPresses :: Int -> System -> System
setPresses newPresses system = system { sysModulesAndRecipients = updatedModulesAndRecipients }
  where
    updatedModulesAndRecipients = M.map updateIfBroadcaster $ sysModulesAndRecipients system
    
    updateIfBroadcaster :: ModuleAndRecipients -> ModuleAndRecipients
    updateIfBroadcaster mar@(ModuleAndRecipients _ (BroadcasterModule _) recips) = 
      mar { marModule = BroadcasterModule (Broadcaster newPresses) }
    updateIfBroadcaster mar = mar

processPulses :: System -> System
processPulses system =
    case getBroadcasterPressCount system of
        0 -> system  -- No more broadcaster presses to process
        _ -> case D.uncons (sysPulses system) of
                Nothing -> processPulses $ decrementBroadcasterPressCount system
                Just (pulse, remainingPulses) -> 
                    let updatedSystem = processPulse pulse system
                    in processPulses $ updatedSystem { sysPulses = remainingPulses }

getBroadcasterPressCount :: System -> Int
getBroadcasterPressCount system =
    case M.lookup "broadcaster" (sysModulesAndRecipients system) of
        Just (ModuleAndRecipients _ (BroadcasterModule (Broadcaster count)) _) -> count
        _ -> 0  -- Fallback case, should not occur if broadcaster always exists

decrementBroadcasterPressCount :: System -> System
decrementBroadcasterPressCount system =
    let modulesAndRecipients = sysModulesAndRecipients system
        updatedModulesAndRecipients = M.map updateBroadcaster modulesAndRecipients
        broadcasterRecips = maybe [] marRecipients $ M.lookup "broadcaster" modulesAndRecipients
        lowPulses = map (\recip -> Pulse { pulseRecipient = recip, isHigh = False }) broadcasterRecips
        updatedPulses = foldl' (flip D.snoc) (sysPulses system) lowPulses
        newProcessedPulses = sysProcessedPulses system ++ lowPulses
    in system { sysModulesAndRecipients = updatedModulesAndRecipients, sysPulses = updatedPulses, sysProcessedPulses = newProcessedPulses }

  where
    updateBroadcaster mar@(ModuleAndRecipients name (BroadcasterModule (Broadcaster count)) recips) =
        if count > 0
        then ModuleAndRecipients name (BroadcasterModule (Broadcaster (count - 1))) recips
        else mar
    updateBroadcaster mar = mar

processPulse :: Pulse -> System -> System
processPulse pulse system =
  let moduleName = pulseRecipient pulse
      moduleAndRecipients = fromJust $ M.lookup moduleName $ sysModulesAndRecipients system
      updatedSystem = case marModule moduleAndRecipients of
                       FlipFlopModule ff -> processFlipFlopPulse pulse ff moduleAndRecipients system
                       ConjuctionModule cn -> processConjuctionPulse pulse cn moduleAndRecipients system
                       BroadcasterModule bc -> processBroadcasterPulse pulse bc moduleAndRecipients system
                       -- Add cases for other module types if necessary
      newProcessedPulses = sysProcessedPulses updatedSystem ++ [pulse]
  in updatedSystem { sysProcessedPulses = newProcessedPulses }

processFlipFlopPulse :: Pulse -> FlipFlop -> ModuleAndRecipients -> System -> System
processFlipFlopPulse pulse ff mar system =
    if isHigh pulse
    then system  -- Ignore high pulses
    else let newFfState = not $ ffIsOn ff
             newPulse = Pulse { pulseRecipient = undefined, isHigh = newFfState }
             updatedPulses = foldl' (\acc r -> D.snoc (newPulse { pulseRecipient = r }) acc) (sysPulses system) (marRecipients mar)
             updatedModule = FlipFlopModule $ FlipFlop newFfState
             updatedModulesAndRecipients = M.insert (marName mar) (ModuleAndRecipients (marName mar) updatedModule (marRecipients mar)) (sysModulesAndRecipients system)
         in system { sysModulesAndRecipients = updatedModulesAndRecipients,
                     sysPulses = updatedPulses }

processConjuctionPulse :: Pulse -> Conjuction -> ModuleAndRecipients -> System -> System
processConjuctionPulse pulse cn mar system =
    let updatedMap = M.insert (pulseRecipient pulse) (isHigh pulse) (cnLastPulseIsHighMap cn)
        allHigh = all id $ M.elems updatedMap
        newPulse = Pulse { pulseRecipient = undefined, isHigh = not allHigh }
        updatedPulses = foldl' (\acc r -> D.snoc (newPulse { pulseRecipient = r }) acc) (sysPulses system) (marRecipients mar)
    in system { sysModulesAndRecipients = M.insert (marName mar) (ModuleAndRecipients (marName mar) (ConjuctionModule $ Conjuction updatedMap) (marRecipients mar)) (sysModulesAndRecipients system),
                sysPulses = updatedPulses }

processBroadcasterPulse :: Pulse -> Broadcaster -> ModuleAndRecipients -> System -> System
processBroadcasterPulse pulse bc mar system =
    let newPulse = Pulse { pulseRecipient = undefined, isHigh = isHigh pulse }
        updatedPulses = foldl' (\acc r -> D.snoc (newPulse { pulseRecipient = r }) acc) (sysPulses system) (marRecipients mar)
    in system { sysPulses = updatedPulses }

countPulses :: [Pulse] -> (Int, Int)
countPulses pulses = 
  let countLowHigh (lows, highs) pulse = if isHigh pulse then (lows, highs + 1) else (lows + 1, highs)
  in foldl' countLowHigh (0, 0) pulses

day20part1 = do
    contents <- readFile "day20 (example).csv"
    let initialSystem = setPresses 1000 $ readSystem $ contents
    let finalSystem = processPulses initialSystem -- assuming initialSystem is your starting state
    let (totalLowPulses, totalHighPulses) = countPulses $ sysProcessedPulses finalSystem
    mapM_ print . M.keys $ sysModulesAndRecipients finalSystem
    putStrLn ""
    mapM_ print . M.elems $ sysModulesAndRecipients finalSystem
    putStrLn ""
    print finalSystem
    
    print totalLowPulses
    print totalHighPulses
