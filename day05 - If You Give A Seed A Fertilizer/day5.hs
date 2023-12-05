#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22 --package fastmemo-0.1.1

--------------------------------
--------------------------------
----  Day 5:  If You Give A Seed A Fertilizer  ----
--------------------------------
--------------------------------
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
-- 


-------------
-- Imports --
-------------
import Data.Char (isDigit)
import Data.List (sort, tails, isPrefixOf, groupBy, find, nub)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)
import Data.IntMap as M hiding (map, filter, take, drop, null)
import Data.Tuple (swap)
import Data.Function (on, fix)
import Data.Function.FastMemo (memoize)


-------------
-- Program --
-------------
main = day5part2

type DestinationAndRangeFromSourceMap = M.IntMap (Int,Int)

data Almanac = Almanac {
    almanacSeedRanges :: [Range],
    almanacSeedToSoilMap            :: DestinationAndRangeFromSourceMap,
    almanacSoilToFertilizerMap      :: DestinationAndRangeFromSourceMap,
    almanacFertilizerToWaterMap     :: DestinationAndRangeFromSourceMap,
    almanacWaterToLightMap          :: DestinationAndRangeFromSourceMap,
    almanacLightToTemperatureMap    :: DestinationAndRangeFromSourceMap,
    almanacTemperatureToHumidityMap :: DestinationAndRangeFromSourceMap,
    almanacHumidityToLocationMap    :: DestinationAndRangeFromSourceMap
    } deriving (Show)

readAlmanac :: String -> Almanac
readAlmanac inStr = Almanac {
    almanacSeedRanges = map (\x -> Interval x 1) . map read . splitOn " " $ seedsStr,
    almanacSeedToSoilMap            = unions . map destinationAndRangeFromSourceMapFromTriple . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nseedToSoilMapStr: "         ++ show x ++ "\n") x)-} $ seedToSoilMapStr,
    almanacSoilToFertilizerMap      = unions . map destinationAndRangeFromSourceMapFromTriple . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nsoilToFertilizerMap: "      ++ show x ++ "\n") x)-} $ soilToFertilizerMap,
    almanacFertilizerToWaterMap     = unions . map destinationAndRangeFromSourceMapFromTriple . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nfertilizerToWaterMap: "     ++ show x ++ "\n") x)-} $ fertilizerToWaterMap,
    almanacWaterToLightMap          = unions . map destinationAndRangeFromSourceMapFromTriple . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nwaterToLightMap: "          ++ show x ++ "\n") x)-} $ waterToLightMap,
    almanacLightToTemperatureMap    = unions . map destinationAndRangeFromSourceMapFromTriple . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nlightToTemperatureMap: "    ++ show x ++ "\n") x)-} $ lightToTemperatureMap,
    almanacTemperatureToHumidityMap = unions . map destinationAndRangeFromSourceMapFromTriple . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\ntemperatureToHumidityMap: " ++ show x ++ "\n") x)-} $ temperatureToHumidityMap,
    almanacHumidityToLocationMap    = unions . map destinationAndRangeFromSourceMapFromTriple . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nhumidityToLocationMap: "    ++ show x ++ "\n") x)-} $ humidityToLocationMap
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
    almanacSeedRanges = seedRangesFromPairs . map read . splitOn " " $ seedsStr,
    almanacSeedToSoilMap            = unions . map destinationAndRangeFromSourceMapFromTriple . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nseedToSoilMapStr: "         ++ show x ++ "\n") x)-} $ seedToSoilMapStr,
    almanacSoilToFertilizerMap      = unions . map destinationAndRangeFromSourceMapFromTriple . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nsoilToFertilizerMap: "      ++ show x ++ "\n") x)-} $ soilToFertilizerMap,
    almanacFertilizerToWaterMap     = unions . map destinationAndRangeFromSourceMapFromTriple . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nfertilizerToWaterMap: "     ++ show x ++ "\n") x)-} $ fertilizerToWaterMap,
    almanacWaterToLightMap          = unions . map destinationAndRangeFromSourceMapFromTriple . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nwaterToLightMap: "          ++ show x ++ "\n") x)-} $ waterToLightMap,
    almanacLightToTemperatureMap    = unions . map destinationAndRangeFromSourceMapFromTriple . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nlightToTemperatureMap: "    ++ show x ++ "\n") x)-} $ lightToTemperatureMap,
    almanacTemperatureToHumidityMap = unions . map destinationAndRangeFromSourceMapFromTriple . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\ntemperatureToHumidityMap: " ++ show x ++ "\n") x)-} $ temperatureToHumidityMap,
    almanacHumidityToLocationMap    = unions . map destinationAndRangeFromSourceMapFromTriple . map (map read) . map (splitOn " ") . lines {-. (\x -> trace ("\nhumidityToLocationMap: "    ++ show x ++ "\n") x)-} $ humidityToLocationMap
    }
  where (seedsStr,                 after1) = break (=='\n') (drop (length "seeds: ") $ inStr)
        (seedToSoilMapStr,         after2) = (\(f,s) -> (take ((length f) - length "\n\nsoil-to-fertilizer map") f, drop (length ":\n") s)) . break (==':') $ (drop (length "\n\nseed-to-soil map:\n") $ after1)
        (soilToFertilizerMap,      after3) = (\(f,s) -> (take ((length f) - length "\n\nfertilizer-to-water map") f, drop (length ":\n") s)) . break (==':') $ after2
        (fertilizerToWaterMap,     after4) = (\(f,s) -> (take ((length f) - length "\n\nwater-to-light map") f, drop (length ":\n") s)) . break (==':') $ after3
        (waterToLightMap,          after5) = (\(f,s) -> (take ((length f) - length "\n\nlight-to-temperature map") f, drop (length ":\n") s)) . break (==':') $ after4
        (lightToTemperatureMap,    after6) = (\(f,s) -> (take ((length f) - length "\n\ntemperature-to-humidity map") f, drop (length ":\n") s)) . break (==':') $ after5
        (temperatureToHumidityMap, after7) = (\(f,s) -> (take ((length f) - length "\n\nhumidity-to-location map") f, drop (length ":\n") s)) . break (==':') $ after6
        humidityToLocationMap = after7

seedRangesFromPairs :: [Int] -> [Range]
seedRangesFromPairs xs = map (\[start,range] -> Interval start range) . chunksOf 2 $ xs

destinationAndRangeFromSourceMapFromTriple :: [Int] -> DestinationAndRangeFromSourceMap
destinationAndRangeFromSourceMapFromTriple [destStart, sourceStart, range] = M.fromList [(sourceStart, (destStart, range))]
destinationAndRangeFromSourceMapFromTriple x = error (show x)

lookupFromMap :: DestinationAndRangeFromSourceMap -> Int -> Int
lookupFromMap m k = case maybeInfimum of
    Nothing -> k
    Just lower -> let (destStart, range) = fromJust $ M.lookup lower m
                  in if k < lower + range
                     then destStart + (k - lower)
                     else k
  where keys = M.keys m
        sortedKeys = sort keys
        lowerKeys = takeWhile (<= k) keys
        maybeInfimum = if null lowerKeys then Nothing else Just $ last lowerKeys

lookupIntervalsFromMap :: DestinationAndRangeFromSourceMap -> Range -> [Range]
lookupIntervalsFromMap m interval = case maybeInfimum of
        Nothing -> k
        Just lower -> let (destStart, range) = fromJust $ M.lookup lower m
                      in if k < lower + range
                         then destStart + (k - lower)
                         else k
  where start = intervalStart interval
        end   = intervalEnd interval
        keys = M.keys m
        sortedKeys = sort keys
        lowerKeys = takeWhile (<= k) keys
        maybeInfimum = if null lowerKeys then Nothing else Just $ last lowerKeys

data Interval = Interval {intervalStart :: Int, intervalRange :: Int}

data Range = Range Interval
           | BoundedBelow Int
           | BoundedAbove Int
           | Unbounded

intervalEnd :: Interval -> Int
intervalEnd i = intervalStart i + intervalRange i - 1

intervalFromStartEnd :: Int -> Int -> Interval
intervalFromStartEnd start end = Interval start (end - start + 1)

locations :: Almanac -> [Int]
locations (Almanac 
    seedRanges
    seedToSoilMap
    soilToFertilizerMap
    fertilizerToWaterMap
    waterToLightMap
    lightToTemperatureMap
    temperatureToHumidityMap
    humidityToLocationMap)
    = flip map seedRanges $ \(Interval start range)@interval -> 
        let end         = intervalEnd interval
            soil        = lookupFromMap seedToSoilMap seed
            fertilizer  = lookupFromMap soilToFertilizerMap soil
            water       = lookupFromMap fertilizerToWaterMap fertilizer
            light       = lookupFromMap waterToLightMap water
            temperature = lookupFromMap lightToTemperatureMap light
            humidity    = lookupFromMap temperatureToHumidityMap temperature
            location    = lookupFromMap humidityToLocationMap humidity
        in location

day5part1 = do
  contents <- readFile "day5 (data).csv"
  let closest = minimum . locations . readAlmanac $ contents
  print $ closest

day5part2 = do
  contents <- readFile "day5 (data).csv"
  let closest = minimum . locations . readAlmanac2 $ contents
  print $ closest