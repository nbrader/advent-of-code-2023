#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5 --package deque-0.4.4.1

-------------------------------------
-------------------------------------
----  Day 20: Pulse Propagation  ----
-------------------------------------
-------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 --package deque-0.4.4.1 -- '.\day20.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day20part1
-- 814934624

-- *Main> day20part2
-- 228282646835717


-------------
-- Imports --
-------------
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe
import Deque.Strict as D
import GHC.Exts as F


-------------
-- Program --
-------------
main = day20part1

-- START ModuleSpec Parse --
data ModuleType = FlipFlopType | ConjunctionType | BroadcasterType deriving (Show, Eq)
data ModuleSpec = ModuleSpec {moduleType :: ModuleType, moduleName :: String, moduleRecipients :: [String]} deriving (Show, Eq)

readModuleType :: String -> ModuleType
readModuleType "%" = FlipFlopType
readModuleType "&" = ConjunctionType
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
data Pulse = Pulse {pulseSender :: String, pulseRecipient :: String, isHigh :: Bool} deriving (Show, Eq)

data FlipFlop = FlipFlop {ffIsOn :: Bool} deriving (Show, Eq)
data Conjunction = Conjunction {cnLastPulseIsHighMap :: M.Map String Bool} deriving (Show, Eq)
data Broadcaster = Broadcaster {bcPresses :: Int} deriving (Show, Eq)
data Module = FlipFlopModule FlipFlop | ConjunctionModule Conjunction | BroadcasterModule Broadcaster deriving (Show, Eq)
data ModuleAndRecipients = ModuleAndRecipients {marName :: String, marModule :: Module, marRecipients :: [String]} deriving (Show, Eq)

data System = System {sysModulesAndRecipients :: M.Map String ModuleAndRecipients, sysPendingPulses :: D.Deque Pulse, sysProcessedPulses :: [Pulse]} deriving (Show, Eq)
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
                    ConjunctionType -> ConjunctionModule (Conjunction existingSenders)
                    BroadcasterType -> BroadcasterModule (Broadcaster 0)
                
                --update all existing ConjunctionType recipients
                newModulesAndRecipients = M.mapWithKey updateIfConjunctionModule oldModulesAndRecipients
                
                updateIfConjunctionModule :: String -> ModuleAndRecipients -> ModuleAndRecipients
                updateIfConjunctionModule recipientName m'@(ModuleAndRecipients _ (ConjunctionModule (Conjunction lastPulseIsHighMap)) _)
                    | recipientName `elem` recipients = m' {marModule = ConjunctionModule (Conjunction (M.insert newModuleName False lastPulseIsHighMap))}
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

processPresses :: System -> [System]
processPresses system = case D.uncons (sysPendingPulses system) of
                            Nothing
                                -> if pressCount == 0
                                    then []
                                    else [broadcast system] ++ processPresses (broadcast system)
                            Just (pulse, remainingPulses)
                                -> let updatedSystem = processPulse pulse (system { sysPendingPulses = remainingPulses })
                                   in processPresses updatedSystem
  where broadcaster = fromJust $ M.lookup "broadcaster" (sysModulesAndRecipients system)
        (ModuleAndRecipients _ (BroadcasterModule (Broadcaster pressCount)) _) = broadcaster

broadcast :: System -> System
broadcast system =
    let modulesAndRecipients = sysModulesAndRecipients system
        updatedModulesAndRecipients = M.map updateBroadcaster modulesAndRecipients
        lowPulse = Pulse { pulseSender = "button", pulseRecipient = "broadcaster", isHigh = False }
        newProcessedPulses = sysProcessedPulses system
    in system { sysModulesAndRecipients = updatedModulesAndRecipients, sysPendingPulses = F.fromList [lowPulse], sysProcessedPulses = newProcessedPulses }

  where
    updateBroadcaster mar@(ModuleAndRecipients name (BroadcasterModule (Broadcaster count)) recips) =
        if count > 0
        then ModuleAndRecipients name (BroadcasterModule (Broadcaster (count - 1))) recips
        else mar
    updateBroadcaster mar = mar

processPulse :: Pulse -> System -> System
processPulse pulse system =
  let moduleName = pulseRecipient pulse
      maybeModuleAndRecipients = M.lookup moduleName $ sysModulesAndRecipients system
      updatedSystem = case maybeModuleAndRecipients of
            Nothing -> system
            Just moduleAndRecipients ->
                      case marModule moduleAndRecipients of
                       FlipFlopModule ff -> processFlipFlopPulse pulse ff moduleAndRecipients system
                       ConjunctionModule cn -> processConjunctionPulse pulse cn moduleAndRecipients system
                       BroadcasterModule bc -> processBroadcasterPulse pulse bc moduleAndRecipients system
      newProcessedPulses = sysProcessedPulses updatedSystem ++ [pulse]
  in updatedSystem { sysProcessedPulses = newProcessedPulses }

processFlipFlopPulse :: Pulse -> FlipFlop -> ModuleAndRecipients -> System -> System
processFlipFlopPulse pulse ff mar system =
    if isHigh pulse
    then system  -- Ignore high pulses
    else let newFfState = not $ ffIsOn ff
             newPulse = Pulse { pulseSender = marName mar, pulseRecipient = undefined, isHigh = newFfState }
             updatedPulses = foldl' (\acc r -> D.snoc (newPulse { pulseRecipient = r }) acc) (sysPendingPulses system) (marRecipients mar)
             updatedModule = FlipFlopModule $ FlipFlop newFfState
             updatedModulesAndRecipients = M.insert (marName mar) (ModuleAndRecipients (marName mar) updatedModule (marRecipients mar)) (sysModulesAndRecipients system)
         in system { sysModulesAndRecipients = updatedModulesAndRecipients,
                     sysPendingPulses = updatedPulses }

processConjunctionPulse :: Pulse -> Conjunction -> ModuleAndRecipients -> System -> System
processConjunctionPulse pulse cn mar system =
    let updatedMap = M.insert (pulseSender pulse) (isHigh pulse) (cnLastPulseIsHighMap cn)
        allHigh = and $ M.elems updatedMap
        newPulse = Pulse { pulseSender = marName mar, pulseRecipient = undefined, isHigh = not allHigh }
        updatedPulses = foldl' (\acc r -> D.snoc (newPulse { pulseRecipient = r }) acc) (sysPendingPulses system) (marRecipients mar)
    in system { sysModulesAndRecipients = M.insert (marName mar) (ModuleAndRecipients (marName mar) (ConjunctionModule $ Conjunction updatedMap) (marRecipients mar)) (sysModulesAndRecipients system),
                sysPendingPulses = updatedPulses }

processBroadcasterPulse :: Pulse -> Broadcaster -> ModuleAndRecipients -> System -> System
processBroadcasterPulse pulse bc mar system =
    let newPulse = Pulse { pulseSender = marName mar, pulseRecipient = undefined, isHigh = isHigh pulse }
        updatedPulses = foldl' (\acc r -> D.snoc (newPulse { pulseRecipient = r }) acc) (sysPendingPulses system) (marRecipients mar)
    in system { sysPendingPulses = updatedPulses }

countPulses :: [Pulse] -> (Int, Int)
countPulses pulses = 
  let countLowHigh (lows, highs) pulse = if isHigh pulse then (lows, highs + 1) else (lows + 1, highs)
  in foldl' countLowHigh (0, 0) pulses

showModules :: System -> String
showModules system = show modulesAndState
  where modulesAndRecipients = M.elems $ sysModulesAndRecipients system
        modulesAndState = map (\x -> (marName x, marModule x)) modulesAndRecipients

day20part1 = do
    contents <- readFile "day20 (example 2).csv"
    let initialSystem = setPresses 1000 $ readSystem $ contents
    let systems = processPresses initialSystem -- assuming initialSystem is your starting state
    -- mapM_ print . M.keys $ sysModulesAndRecipients finalSystem
    -- putStrLn ""
    -- mapM_ print . M.elems $ sysModulesAndRecipients finalSystem
    -- putStrLn ""
    -- print initialSystem
    -- putStrLn ""
    -- print finalSystem
    
    -- mapM_ (putStrLn . showModules) systems
    
    let modulesSpecs = map readModuleSpec . lines $ contents
    putStrLn . showAsTGF $ modulesSpecs

showAsTGF :: [ModuleSpec] -> String
showAsTGF moduleSpecs = unlines $ concat [nodeRows, ["#"], edgeRows]
  where nodeRows, edgeRows :: [String]
        nodeRows =          map (\spec -> moduleName spec ++ " " ++ show (moduleType spec) ++ " " ++ moduleName spec) $ moduleSpecs
        edgeRows = concat $ map (\spec -> map unwords $ (sequence [([moduleName spec]), (moduleRecipients spec)])) $ moduleSpecs


bitMaskA   = sum $ zipWith (\i c -> c*2^i) [0..] [1,1,1,1,0,0,1,0,1,1,1,1]
incrementA = sum $ zipWith (\i c -> c*2^i) [0..] [1,0,0,0,1,1,0,1,0,0,0,0] -- This is just resetting

bitMaskB   = sum $ zipWith (\i c -> c*2^i) [0..] [1,0,0,0,1,1,0,1,0,1,1,1]
incrementB = sum $ zipWith (\i c -> c*2^i) [0..] [1,1,1,1,0,0,1,0,1,0,0,0] -- This is just resetting

bitMaskC   = sum $ zipWith (\i c -> c*2^i) [0..] [1,0,1,0,1,0,1,1,0,1,1,1]
incrementC = sum $ zipWith (\i c -> c*2^i) [0..] [1,1,0,1,0,1,0,0,1,0,0,0] -- This is just resetting

bitMaskD   = sum $ zipWith (\i c -> c*2^i) [0..] [1,1,1,1,0,1,1,1,1,1,1,1]
incrementD = sum $ zipWith (\i c -> c*2^i) [0..] [1,0,0,0,1,0,0,0,0,0,0,0] -- This is just resetting

day20part2 = foldr lcm 1 [bitMaskA,bitMaskB,bitMaskC,bitMaskD]