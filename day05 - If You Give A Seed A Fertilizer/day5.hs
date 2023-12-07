#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22 --package fastmemo-0.1.1

---------------------------------------------------
---------------------------------------------------
----  Day 5:  If You Give A Seed A Fertilizer  ----
---------------------------------------------------
---------------------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22 -- '.\day5.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day5part1
-- 

-- *Main> day5part2
-- 69841803


-------------
-- Imports --
-------------
import Data.Char (isDigit)
import Data.List (sort, tails, isPrefixOf, groupBy, find, nub, foldl')
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)
import Data.Tuple (swap)
import Data.Function (on, fix)
import Data.Function.FastMemo (memoize)


-------------
-- Program --
-------------
main = day5part2

data IntervalMap = IntervalMap {fromIntervals :: [Interval], toIntervals :: [Interval]} deriving (Show)
emptyIntervalMap = IntervalMap [] []

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
    almanacSeedToSoilMap            = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nseedToSoilMapStr: "         ++ show x ++ "\n") x)-} $ seedToSoilMapStr,
    almanacSoilToFertilizerMap      = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nsoilToFertilizerMap: "      ++ show x ++ "\n") x)-} $ soilToFertilizerMap,
    almanacFertilizerToWaterMap     = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nfertilizerToWaterMap: "     ++ show x ++ "\n") x)-} $ fertilizerToWaterMap,
    almanacWaterToLightMap          = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nwaterToLightMap: "          ++ show x ++ "\n") x)-} $ waterToLightMap,
    almanacLightToTemperatureMap    = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nlightToTemperatureMap: "    ++ show x ++ "\n") x)-} $ lightToTemperatureMap,
    almanacTemperatureToHumidityMap = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\ntemperatureToHumidityMap: " ++ show x ++ "\n") x)-} $ temperatureToHumidityMap,
    almanacHumidityToLocationMap    = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nhumidityToLocationMap: "    ++ show x ++ "\n") x)-} $ humidityToLocationMap
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
    almanacSeedToSoilMap            = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nseedToSoilMapStr: "         ++ show x ++ "\n") x)-} $ seedToSoilMapStr,
    almanacSoilToFertilizerMap      = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nsoilToFertilizerMap: "      ++ show x ++ "\n") x)-} $ soilToFertilizerMap,
    almanacFertilizerToWaterMap     = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nfertilizerToWaterMap: "     ++ show x ++ "\n") x)-} $ fertilizerToWaterMap,
    almanacWaterToLightMap          = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nwaterToLightMap: "          ++ show x ++ "\n") x)-} $ waterToLightMap,
    almanacLightToTemperatureMap    = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nlightToTemperatureMap: "    ++ show x ++ "\n") x)-} $ lightToTemperatureMap,
    almanacTemperatureToHumidityMap = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\ntemperatureToHumidityMap: " ++ show x ++ "\n") x)-} $ temperatureToHumidityMap,
    almanacHumidityToLocationMap    = foldl' addIntervalFromTriple emptyIntervalMap . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nhumidityToLocationMap: "    ++ show x ++ "\n") x)-} $ humidityToLocationMap
    }
  where (seedsStr,                 after1) = break (=='\n') (drop (length "seeds: ") $ inStr)
        (seedToSoilMapStr,         after2) = (\(f,s) -> (take ((length f) - length "\n\nsoil-to-fertilizer map") f, drop (length ":\n") s)) . break (==':') $ (drop (length "\n\nseed-to-soil map:\n") $ after1)
        (soilToFertilizerMap,      after3) = (\(f,s) -> (take ((length f) - length "\n\nfertilizer-to-water map") f, drop (length ":\n") s)) . break (==':') $ after2
        (fertilizerToWaterMap,     after4) = (\(f,s) -> (take ((length f) - length "\n\nwater-to-light map") f, drop (length ":\n") s)) . break (==':') $ after3
        (waterToLightMap,          after5) = (\(f,s) -> (take ((length f) - length "\n\nlight-to-temperature map") f, drop (length ":\n") s)) . break (==':') $ after4
        (lightToTemperatureMap,    after6) = (\(f,s) -> (take ((length f) - length "\n\ntemperature-to-humidity map") f, drop (length ":\n") s)) . break (==':') $ after5
        (temperatureToHumidityMap, after7) = (\(f,s) -> (take ((length f) - length "\n\nhumidity-to-location map") f, drop (length ":\n") s)) . break (==':') $ after6
        humidityToLocationMap = after7

seedIntervalsFromPairs :: [Int] -> [Interval]
seedIntervalsFromPairs xs = map (\[start,range] -> intervalFromStartAndRange start range) . chunksOf 2 $ xs

addIntervalFromTriple :: IntervalMap -> [Int] -> IntervalMap
addIntervalFromTriple (IntervalMap from to) [destStart, sourceStart, range] = IntervalMap (intervalFromStartAndRange sourceStart range:from) (intervalFromStartAndRange destStart range:to)
addIntervalFromTriple x y = error (show (x,y))

-- lookupFromMap :: DestinationAndRangeFromSourceMap -> Int -> Int
-- lookupFromMap m k = case maybeInfimum of
    -- Nothing -> k
    -- Just lower -> let (destStart, range) = fromJust $ M.lookup lower m
                  -- in if k < lower + range
                     -- then destStart + (k - lower)
                     -- else k
  -- where keys = M.keys m
        -- sortedKeys = sort keys
        -- lowerKeys = takeWhile (<= k) keys
        -- maybeInfimum = if null lowerKeys then Nothing else Just $ last lowerKeys

-- lookupIntervalsFromMap :: DestinationAndRangeFromSourceMap -> Interval -> [Interval]
-- lookupIntervalsFromMap m (Interval start end)
    -- = let start = intervalStart interval
          -- end   = intervalEnd interval
          -- lookupFromMap

applyIntervalMap :: [Interval] -> IntervalMap -> [Interval]
applyIntervalMap intervals m = concatMap (applyMap m) intervals
  where applyMap :: IntervalMap -> Interval -> [Interval]
        applyMap (IntervalMap [] [])           interval = [interval]
        applyMap (IntervalMap (f:from) (t:to)) interval = concatMap (applyMap (IntervalMap from to)) (applyInterval (f,t) interval)
        
        applyInterval :: (Interval,Interval) -> Interval -> [Interval]
        applyInterval (Interval sourceStart sourceEnd, Interval destinationStart destinationEnd) (Interval start end)
            | end   <  sourceStart || start >  sourceEnd = [Interval start end]
            | start <  sourceStart && end   <= sourceEnd = [Interval start (sourceStart-1), Interval destinationStart (end + destinationStart - sourceStart)]
            | start <  sourceStart && end   >  sourceEnd = [Interval start (sourceStart-1), Interval destinationStart destinationEnd, Interval (sourceEnd+1) end]
            | start == sourceStart && end   <= sourceEnd = [Interval destinationStart (end + destinationStart - sourceStart)]
            | start == sourceStart && end   >  sourceEnd = [Interval destinationStart destinationEnd, Interval (sourceEnd+1) end]
            | start >  sourceStart && end   <= sourceEnd = [Interval (start + destinationStart - sourceStart) (end + destinationStart - sourceStart)]
            | otherwise                                  = [Interval (start + destinationStart - sourceStart) destinationEnd, Interval (sourceEnd+1) end]

data Interval = Interval {intervalStart :: Int, intervalEnd :: Int} deriving (Show)

intervalFromStartAndRange :: Int -> Int -> Interval
intervalFromStartAndRange start range = Interval start (start + range - 1)

locations :: Almanac -> [Int]
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