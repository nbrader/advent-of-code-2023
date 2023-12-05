#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22 --package fastmemo-0.1.1

--------------------------------
--------------------------------
----  Day 5:  If You Give A Seed A Fertilizer  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22
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
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)
import Data.Map as M hiding (map, filter, take, drop)
import Data.Tuple (swap)
import Data.Function (on, fix)
import Data.Function.FastMemo (memoize)


-------------
-- Program --
-------------
main = day5part1

data Almanac = Almanac {
    almanacSeeds :: [Integer],
    almanacSeedToSoilMap            :: M.Map Integer Integer,
    almanacSoilToFertilizerMap      :: M.Map Integer Integer,
    almanacFertilizerToWaterMap     :: M.Map Integer Integer,
    almanacWaterToLightMap          :: M.Map Integer Integer,
    almanacLightToTemperatureMap    :: M.Map Integer Integer,
    almanacTemperatureToHumidityMap :: M.Map Integer Integer,
    almanacHumidityToLocationMap    :: M.Map Integer Integer
    } deriving (Show)

readAlmanac :: String -> Almanac
readAlmanac inStr = Almanac {
    almanacSeeds = map read . splitOn " " $ seedsStr,
    almanacSeedToSoilMap            = unions . map mapFromTriple . map (map read) . map (splitOn " ") . lines $ seedToSoilMapStr,
    almanacSoilToFertilizerMap      = unions . map mapFromTriple . map (map read) . map (splitOn " ") . lines $ soilToFertilizerMap,
    almanacFertilizerToWaterMap     = unions . map mapFromTriple . map (map read) . map (splitOn " ") . lines $ fertilizerToWaterMap,
    almanacWaterToLightMap          = unions . map mapFromTriple . map (map read) . map (splitOn " ") . lines $ waterToLightMap,
    almanacLightToTemperatureMap    = unions . map mapFromTriple . map (map read) . map (splitOn " ") . lines $ lightToTemperatureMap,
    almanacTemperatureToHumidityMap = unions . map mapFromTriple . map (map read) . map (splitOn " ") . lines $ temperatureToHumidityMap,
    almanacHumidityToLocationMap    = unions . map mapFromTriple . map (map read) . map (splitOn " ") . lines $ humidityToLocationMap
    }
  where (seedsStr,                 after1) = break (=='\n') (drop (length "seeds: ") $ inStr)
        (seedToSoilMapStr,         after2) = break (==':') (drop (length "\n\nseed-to-soil map:\n") $ after1)
        (soilToFertilizerMap,      after3) = break (==':') (drop (length "\n\nsoil-to-fertilizer map:\n") $ after2)
        (fertilizerToWaterMap,     after4) = break (==':') (drop (length "\n\nfertilizer-to-water map:\n") $ after3)
        (waterToLightMap,          after5) = break (==':') (drop (length "\n\nwater-to-light map:\n") $ after4)
        (lightToTemperatureMap,    after6) = break (==':') (drop (length "\n\nlight-to-temperature map:\n") $ after5)
        (temperatureToHumidityMap, after7) = break (==':') (drop (length "\n\ntemperature-to-humidity map:\n") $ after6)
        humidityToLocationMap =                            (drop (length "\n\nhumidity-to-location map:\n") $ after7)

mapFromTriple :: [Integer] -> Map Integer Integer
mapFromTriple [destStart, sourceStart, amount] = M.fromList [(sourceStart + i, destStart + i) | i <- take (fromIntegral amount) [0..]]

lookupFromMap = (\m x -> fromMaybe x $ M.lookup x m)

locations :: Almanac -> [Integer]
locations (Almanac 
    seeds
    seedToSoilMap
    soilToFertilizerMap
    fertilizerToWaterMap
    waterToLightMap
    lightToTemperatureMap
    temperatureToHumidityMap
    humidityToLocationMap)
    = flip map seeds $ \seed -> 
        let soil        = lookupFromMap seedToSoilMap seed
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