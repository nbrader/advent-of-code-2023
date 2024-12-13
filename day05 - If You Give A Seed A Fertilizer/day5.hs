#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package split-0.2.3.5 --package containers-0.6.7 --package fastmemo-0.1.1

---------------------------------------------------
---------------------------------------------------
----  Day 5:  If You Give A Seed A Fertilizer  ----
---------------------------------------------------
---------------------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package split-0.2.3.5 --package containers-0.6.7 -- '.\day5.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day5part1
-- 177942185

-- *Main> day5part2
-- 69841803


-------------
-- Imports --
-------------
import Data.List (find, foldl')
import Data.List.Split (splitOn, chunksOf)
import Data.Map as M hiding (map, take, drop, foldl')


-------------
-- Program --
-------------
main = day5part2

type IntervalMap = Map Interval Interval
emptyIntervalMap = M.fromList []

data Almanac = Almanac {
    almanacSeedIntervals :: [Interval],
    almanacSeedToSoilMap            :: IntervalMap,
    almanacSoilToFertilizerMap      :: IntervalMap,
    almanacFertilizerToWaterMap     :: IntervalMap,
    almanacWaterToLightMap          :: IntervalMap,
    almanacLightToTemperatureMap    :: IntervalMap,
    almanacTemperatureToHumidityMap :: IntervalMap,
    almanacHumidityToLocationMap    :: IntervalMap
    } deriving (Show)

readAlmanac :: String -> Almanac
readAlmanac inStr = Almanac {
    almanacSeedIntervals = map (\x -> Interval x x) . map read . splitOn " " $ seedsStr,
    almanacSeedToSoilMap            = readIntervalMap seedToSoilMapStr,
    almanacSoilToFertilizerMap      = readIntervalMap soilToFertilizerMap,
    almanacFertilizerToWaterMap     = readIntervalMap fertilizerToWaterMap,
    almanacWaterToLightMap          = readIntervalMap waterToLightMap,
    almanacLightToTemperatureMap    = readIntervalMap lightToTemperatureMap,
    almanacTemperatureToHumidityMap = readIntervalMap temperatureToHumidityMap,
    almanacHumidityToLocationMap    = readIntervalMap humidityToLocationMap
    }
  where (seedsStr,                 after1) = break (=='\n') (drop (length "seeds: ") $ inStr)
        (seedToSoilMapStr,         after2) = (\(f,s) -> (take ((length f) - length "\n\nsoil-to-fertilizer map") f, drop (length ":\n") s)) . break (==':') $ (drop (length "\n\nseed-to-soil map:\n") $ after1)
        (soilToFertilizerMap,      after3) = (\(f,s) -> (take ((length f) - length "\n\nfertilizer-to-water map") f, drop (length ":\n") s)) . break (==':') $ after2
        (fertilizerToWaterMap,     after4) = (\(f,s) -> (take ((length f) - length "\n\nwater-to-light map") f, drop (length ":\n") s)) . break (==':') $ after3
        (waterToLightMap,          after5) = (\(f,s) -> (take ((length f) - length "\n\nlight-to-temperature map") f, drop (length ":\n") s)) . break (==':') $ after4
        (lightToTemperatureMap,    after6) = (\(f,s) -> (take ((length f) - length "\n\ntemperature-to-humidity map") f, drop (length ":\n") s)) . break (==':') $ after5
        (temperatureToHumidityMap, after7) = (\(f,s) -> (take ((length f) - length "\n\nhumidity-to-location map") f, drop (length ":\n") s)) . break (==':') $ after6
        humidityToLocationMap = after7

readAlmanac2 :: String -> Almanac
readAlmanac2 inStr = Almanac {
    almanacSeedIntervals = seedIntervalsFromPairs . map read . splitOn " " $ seedsStr,
    almanacSeedToSoilMap            = readIntervalMap seedToSoilMapStr,
    almanacSoilToFertilizerMap      = readIntervalMap soilToFertilizerMap,
    almanacFertilizerToWaterMap     = readIntervalMap fertilizerToWaterMap,
    almanacWaterToLightMap          = readIntervalMap waterToLightMap,
    almanacLightToTemperatureMap    = readIntervalMap lightToTemperatureMap,
    almanacTemperatureToHumidityMap = readIntervalMap temperatureToHumidityMap,
    almanacHumidityToLocationMap    = readIntervalMap humidityToLocationMap
    }
  where (seedsStr,                 after1) = break (=='\n') (drop (length "seeds: ") $ inStr)
        (seedToSoilMapStr,         after2) = (\(f,s) -> (take ((length f) - length "\n\nsoil-to-fertilizer map") f, drop (length ":\n") s)) . break (==':') $ (drop (length "\n\nseed-to-soil map:\n") $ after1)
        (soilToFertilizerMap,      after3) = (\(f,s) -> (take ((length f) - length "\n\nfertilizer-to-water map") f, drop (length ":\n") s)) . break (==':') $ after2
        (fertilizerToWaterMap,     after4) = (\(f,s) -> (take ((length f) - length "\n\nwater-to-light map") f, drop (length ":\n") s)) . break (==':') $ after3
        (waterToLightMap,          after5) = (\(f,s) -> (take ((length f) - length "\n\nlight-to-temperature map") f, drop (length ":\n") s)) . break (==':') $ after4
        (lightToTemperatureMap,    after6) = (\(f,s) -> (take ((length f) - length "\n\ntemperature-to-humidity map") f, drop (length ":\n") s)) . break (==':') $ after5
        (temperatureToHumidityMap, after7) = (\(f,s) -> (take ((length f) - length "\n\nhumidity-to-location map") f, drop (length ":\n") s)) . break (==':') $ after6
        humidityToLocationMap = after7

readIntervalMap :: String -> IntervalMap
readIntervalMap = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines

seedIntervalsFromPairs :: [Int] -> [Interval]
seedIntervalsFromPairs xs = map (\[start,range] -> intervalFromStartAndRange start range) . chunksOf 2 $ xs

addIntervalFromTriple :: IntervalMap -> [Int] -> IntervalMap
addIntervalFromTriple intervalMap [destStart, sourceStart, range] = M.insert (intervalFromStartAndRange sourceStart range) (intervalFromStartAndRange destStart range) intervalMap
addIntervalFromTriple x y = error (show (x,y))

isSubInterval :: Interval -> Interval -> Bool
isSubInterval sub super = intervalStart super <= intervalStart sub && intervalEnd sub <= intervalEnd super

applyIntervalMap :: [Interval] -> IntervalMap -> [Interval]
applyIntervalMap intervals intervalMap = concat . map (applySourceInterval . assocIntervalsToSourceInterval) . refineByMap $ intervals
  where refineByMap :: [Interval] -> [Interval]
        refineByMap intervals = foldl' (\intervals' sourceInterval -> concat $ map (refineByInterval sourceInterval) intervals') intervals (M.keys intervalMap)
        
        assocIntervalsToSourceInterval :: Interval -> (Interval, Maybe Interval)
        assocIntervalsToSourceInterval interval = (interval, find (\fromInterval -> interval `isSubInterval` fromInterval) (M.keys intervalMap))
        
        refineByInterval :: Interval -> Interval -> [Interval]
        refineByInterval (Interval sourceStart sourceEnd) (Interval start end)
            | end   <  sourceStart || start >  sourceEnd = [Interval start end]
            | start <  sourceStart && end   <= sourceEnd = [Interval start (sourceStart-1), Interval sourceStart end]
            | start <  sourceStart && end   >  sourceEnd = [Interval start (sourceStart-1), Interval sourceStart sourceEnd, Interval (sourceEnd+1) end]
            | start == sourceStart && end   <= sourceEnd = [Interval sourceStart end]
            | start == sourceStart && end   >  sourceEnd = [Interval sourceStart sourceEnd, Interval (sourceEnd+1) end]
            | start >  sourceStart && end   <= sourceEnd = [Interval start end]
            | otherwise                                  = [Interval start sourceEnd, Interval (sourceEnd+1) end]
        
        applySourceInterval :: (Interval, Maybe Interval) -> [Interval]
        applySourceInterval (interval, Nothing) = [interval]
        applySourceInterval (interval, Just sourceInterval) = applyInterval (sourceInterval, destinationInterval) interval
          where Just destinationInterval = M.lookup sourceInterval intervalMap
        
        applyInterval :: (Interval,Interval) -> Interval -> [Interval]
        applyInterval (Interval sourceStart sourceEnd, Interval destinationStart destinationEnd) (Interval start end)
            | end   <  sourceStart || start >  sourceEnd = [Interval start end]
            | start <  sourceStart && end   <= sourceEnd = [Interval start (sourceStart-1), Interval destinationStart (end + destinationStart - sourceStart)]
            | start <  sourceStart && end   >  sourceEnd = [Interval start (sourceStart-1), Interval destinationStart destinationEnd, Interval (sourceEnd+1) end]
            | start == sourceStart && end   <= sourceEnd = [Interval destinationStart (end + destinationStart - sourceStart)]
            | start == sourceStart && end   >  sourceEnd = [Interval destinationStart destinationEnd, Interval (sourceEnd+1) end]
            | start >  sourceStart && end   <= sourceEnd = [Interval (start + destinationStart - sourceStart) (end + destinationStart - sourceStart)]
            | otherwise                                  = [Interval (start + destinationStart - sourceStart) destinationEnd, Interval (sourceEnd+1) end]

data Interval = Interval {intervalStart :: Int, intervalEnd :: Int} deriving (Show, Eq, Ord)

intervalFromStartAndRange :: Int -> Int -> Interval
intervalFromStartAndRange start range = Interval start (start + range - 1)

locations (Almanac 
    seedIntervals
    seedToSoilMap
    soilToFertilizerMap
    fertilizerToWaterMap
    waterToLightMap
    lightToTemperatureMap
    temperatureToHumidityMap
    humidityToLocationMap)
    = let locationIntervals = foldl' applyIntervalMap seedIntervals [
            seedToSoilMap,
            soilToFertilizerMap,
            fertilizerToWaterMap,
            waterToLightMap,
            lightToTemperatureMap,
            temperatureToHumidityMap,
            humidityToLocationMap
            ]
      in map intervalStart locationIntervals

day5part1 = do
  contents <- readFile "day5 (data).csv"
  let closest = minimum . locations . readAlmanac $ contents
  print $ closest

day5part2 = do
  contents <- readFile "day5 (data).csv"
  let closest = minimum . locations . readAlmanac2 $ contents
  print $ closest

expandInterval :: Interval -> [Int]
expandInterval interval = [intervalStart interval .. intervalEnd interval]