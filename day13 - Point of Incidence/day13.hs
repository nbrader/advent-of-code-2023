#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package split-0.2.3.5

---------------------------------------
---------------------------------------
----  Day 13:  Point of Incidence  ----
---------------------------------------
---------------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package split-0.2.3.5 -- -threaded -O2 '.\day13.hs'
-}

------------
-- Output --
------------
-- *Main> day13part1
-- 27505

-- *Main> day13part2
-- 22906


-------------
-- Imports --
-------------
import Data.List (transpose, findIndices, intercalate, reverse, length, delete)
import Data.List.Split (splitOn, chunksOf)
import Data.Bits


-------------
-- Program --
-------------
main = day13part1

data RockPattern = RockPattern {rockPatternInt :: Integer, rockPatternRowLength :: Int, rockPatternRowAmount :: Int} deriving (Show)
newtype RockPatternString = RockPatternString {fromRockPatternString :: String} deriving (Show)

readRockPatternStrings :: String -> [RockPatternString]
readRockPatternStrings = map RockPatternString . splitOn "\n\n"

readRockPattern :: String -> RockPattern
readRockPattern inStr = RockPattern pattern rowLength rowAmount
  where rows = lines inStr
        
        rowAmount = length rows
        rowLength = length (head rows)
        pattern = sum $ zipWith (\i digit -> digit*2^i) [0..] . map (\c -> if c == '#' then 1 else 0) $ concat rows

allPatternDirs :: RockPatternString -> (RockPattern, RockPattern, RockPattern, RockPattern)
allPatternDirs (RockPatternString s) = (rockPatternDn, rockPatternUp, rockPatternLt, rockPatternRt)
  where rockPatternDn = readRockPattern . id                                    $ s
        rockPatternUp = readRockPattern . unlines . reverse             . lines $ s
        rockPatternLt = readRockPattern . unlines .           transpose . lines $ s
        rockPatternRt = readRockPattern . unlines . reverse . transpose . lines $ s

-- printAllDirs (rockPatternDn, rockPatternUp, rockPatternLt, rockPatternRt) = do
    -- print rockPatternDn
    -- print rockPatternUp
    -- print rockPatternLt
    -- print rockPatternRt
    -- putStrLn ""

-- day13part1 = do
  -- contents <- readFile "day13 (example).csv"
  -- mapM_ printAllDirs . map allPatternDirs . readRockPatternStrings $ contents

findHorizontalReflectionIndices :: RockPatternString -> [Int]
findHorizontalReflectionIndices (RockPatternString s) = map toCountOfBehindReflectionLine . filter reflectionLineBetweenRocks . findIndices (==0) $ allXorsBetween rowLength rowAmount (rockPatternInt rockPatternDn) (rockPatternInt rockPatternUp)
  where rockPatternDn = readRockPattern . id                                    $ s
        rockPatternUp = readRockPattern . unlines . reverse             . lines $ s
        
        rowAmount = rockPatternRowAmount rockPatternDn
        rowLength = rockPatternRowLength rockPatternDn

findVerticalReflectionIndices :: RockPatternString -> [Int]
findVerticalReflectionIndices (RockPatternString s) = map toCountOfBehindReflectionLine . filter reflectionLineBetweenRocks . findIndices (==0) $ allXorsBetween rowLength rowAmount (rockPatternInt rockPatternLt) (rockPatternInt rockPatternRt)
  where rockPatternLt = readRockPattern . unlines .           transpose . lines $ s
        rockPatternRt = readRockPattern . unlines . reverse . transpose . lines $ s
        
        rowAmount = rockPatternRowAmount rockPatternLt
        rowLength = rockPatternRowLength rockPatternLt

toCountOfBehindReflectionLine :: Int -> Int
toCountOfBehindReflectionLine = (+1) . (`div` 2)

reflectionLineBetweenRocks :: Int -> Bool
reflectionLineBetweenRocks = (==0) . (`mod` 2)

truncateBits :: Int -> Integer -> Integer
truncateBits digits x = x .&. ((1 `shiftL` digits) - 1)

allPairsBetween :: Int -> Int -> Integer -> Integer -> [(Integer,Integer)]
allPairsBetween rowLength numOfRows rockInt1 rockInt2 = left ++ right
  where left  = [(chop rockInt1, shiftR rockInt2 delta) | i <- [(numOfRows-2), (numOfRows-3) .. 1], let delta = i*rowLength, let chop = truncateBits ((numOfRows - i)*rowLength)]
        right = [(shiftR rockInt1 delta, chop rockInt2) | i <- [0..(numOfRows-2)],                  let delta = i*rowLength, let chop = truncateBits ((numOfRows - i)*rowLength)]

allXorsBetween :: Int -> Int -> Integer -> Integer -> [Integer]
allXorsBetween rowLength numOfRows rockInt1 rockInt2 = [l `xor` r | (l,r) <- allPairsBetween rowLength numOfRows rockInt1 rockInt2]

allPairsBetweenAsRockPatterns :: RockPattern -> RockPattern -> [(RockPattern, RockPattern)]
allPairsBetweenAsRockPatterns rp1 rp2 = [(RockPattern l rowLength rowAmount, RockPattern r rowLength rowAmount) | (l,r) <- allPairsBetween rowLength rowAmount (rockPatternInt rp1) (rockPatternInt rp2)]
  where rowAmount = rockPatternRowAmount rp1
        rowLength = rockPatternRowLength rp1

allXorsBetweenAsRockPatterns :: RockPattern -> RockPattern -> [RockPattern]
allXorsBetweenAsRockPatterns rp1 rp2 = [RockPattern rocksInt rowLength rowAmount | rocksInt <- allXorsBetween rowLength rowAmount (rockPatternInt rp1) (rockPatternInt rp2)]
  where rowAmount = rockPatternRowAmount rp1
        rowLength = rockPatternRowLength rp1

-- allEvenXorsBetween :: Int -> Int -> Integer -> Integer -> [Integer]
-- allEvenXorsBetween rowLength numOfRows rockInt1 rockInt2 = left ++ right
  -- where left  = [(chop $ rockInt1) `xor` shiftR rockInt2 delta | i <- [(numOfRows-2), (numOfRows-3) .. 1], let height = (numOfRows - i), height `mod` 2 == 0, let delta = i*rowLength, let chop = truncateBits (height*rowLength)]
        -- right = [shiftR rockInt1 delta `xor` (chop $ rockInt2) | i <- [0..(numOfRows-2)],                  let height = (numOfRows - i), height `mod` 2 == 0, let delta = i*rowLength, let chop = truncateBits (height*rowLength)]

toBin :: Int -> Integer -> String
toBin bitCount n = [if n `testBit` i then '#' else '.' | i <- [0..(bitCount-1)]]

showRockPattern :: RockPattern -> String
showRockPattern rp = (unlines . chunksOf rowLength . toBin (rowLength * rowAmount)) (rockPatternInt rp)
  where rowAmount = rockPatternRowAmount rp
        rowLength = rockPatternRowLength rp

prettyPrintAllDirs (rockPatternDn, rockPatternUp, rockPatternLt, rockPatternRt) = do
    print rockPatternDn
    putStrLn . showRockPattern $ rockPatternDn
    putStrLn ""
    print rockPatternUp
    putStrLn . showRockPattern $ rockPatternUp
    putStrLn ""
    putStrLn "^V^V^V^V^V^V"
    putStrLn ""
    mapM_ (\((l,r),x) -> (putStrLn . showRockPattern $ l) >> putStrLn "====" >> (putStrLn . showRockPattern $ r) >> putStrLn "=====" >> (putStrLn . showRockPattern $ x)) . map snd . filter (reflectionLineBetweenRocks . fst) . zip [0..] $ (allPairsBetweenAsRockPatterns rockPatternDn rockPatternUp `zip` allXorsBetweenAsRockPatterns rockPatternDn rockPatternUp)
    putStrLn ""
    putStrLn "------------"
    putStrLn ""
    print rockPatternLt
    putStrLn . showRockPattern $ rockPatternLt
    putStrLn ""
    print rockPatternRt
    putStrLn . showRockPattern $ rockPatternRt
    putStrLn ""
    putStrLn "^V^V^V^V^V^V"
    putStrLn ""
    mapM_ (\((l,r),x) -> (putStrLn . showRockPattern $ l) >> putStrLn "====" >> (putStrLn . showRockPattern $ r) >> putStrLn "=====" >> (putStrLn . showRockPattern $ x)) . map snd . filter (reflectionLineBetweenRocks . fst) . zip [0..] $ (allPairsBetweenAsRockPatterns rockPatternLt rockPatternRt `zip` allXorsBetweenAsRockPatterns rockPatternLt rockPatternRt)
    putStrLn ""
    putStrLn "------------"
    putStrLn "############"
    putStrLn "------------"
    putStrLn ""

day13part1 = do
  contents <- readFile "day13 (example).csv"
  let rockPatternStrings = readRockPatternStrings contents
  let colSum = sum . map sum $ map findVerticalReflectionIndices rockPatternStrings
  let rowSum = sum . map sum $ map findHorizontalReflectionIndices rockPatternStrings
  print (100 * rowSum + colSum)

getAllSmudgeVariations :: RockPatternString -> [RockPatternString]
getAllSmudgeVariations (RockPatternString s) = map RockPatternString $ [before ++ c':after | i <- [0..(length s - 1)], let (before,c:after) = splitAt i s, c `elem` "#.", let c' = if c=='.' then '#' else '.']

day13part2 = do
  contents <- readFile "day13 (data).csv"
  let rockPatternStrings = readRockPatternStrings contents
  let colIndicesNoSmudge = map findVerticalReflectionIndices rockPatternStrings
  let rowIndicesNoSmudge = map findHorizontalReflectionIndices rockPatternStrings
  
  let rockPatternStringsWithSmudgeVariations = map getAllSmudgeVariations rockPatternStrings
  let colIndicesWithSmudge = map (map findVerticalReflectionIndices) rockPatternStringsWithSmudgeVariations
  let rowIndicesWithSmudge = map (map findHorizontalReflectionIndices) rockPatternStringsWithSmudgeVariations
  
  let colIndices = concat . concat $ zipWith (\is jss -> map (\js -> foldr delete js is) jss) colIndicesNoSmudge colIndicesWithSmudge
  let rowIndices = concat . concat $ zipWith (\is jss -> map (\js -> foldr delete js is) jss) rowIndicesNoSmudge rowIndicesWithSmudge
  
  let colSum = sum colIndices
  let rowSum = sum rowIndices
  
  print $ (100 * rowSum + colSum) `div` 2 -- Not sure where this factor of two came from... I must be double counting somehow

test1 = do
  let contents = intercalate "\n\n" [testStr1,testStr2,testStr3]
  let rockPatternStrings = readRockPatternStrings contents
  let rockPatternsAllDirs = map allPatternDirs $ rockPatternStrings
  mapM_ (putStrLn . fromRockPatternString) rockPatternStrings
  mapM_ prettyPrintAllDirs rockPatternsAllDirs

-- Given allXorsBetween gives index for a reflection about the line between rows or on a row starting between the first two rows and ending between the last two rows, we should get the following:
-- testStr1 = unlines [".  #  #  .  .  #  #  .  .",
--                                                  0
--                     ".  #  .  #  #  .  #  .  .", 1
--                                                  2
--                     "#  .  .  .  .  .  .  #  .", 3
--                                                  4 <---- Reflection
--                     "#  .  .  .  .  .  .  #  .", 5
--                                                  6
--                     ".  #  .  #  #  .  #  .  .", 7
--                                                  8
--                     ".  #  #  .  .  #  #  .  .", 9
--                                                  10
--                     ".  #  .  #  #  .  #  .  ."] 
--                         1  3  5  7  9  11 13     
--                       0  2  4  6  8  10 12 14    
--                                ^                    
--                                |                    
--                                |                    
--                                |                    
--                                |                    
--                                Reflection           
-- 
--  ([[4]],[[6]])
-- 
-- The following code does not match this expectation:
--  ([[5,10]],[[7]])
--
-- And the other inputs below also come out wrong
--
tests = do
  let rockPatternStrings = readRockPatternStrings (intercalate "\n\n" [testStr1,testStr2,testStr3])
  let colSum = map findVerticalReflectionIndices $ rockPatternStrings
  let rowSum = map findHorizontalReflectionIndices $ rockPatternStrings
  mapM_ (putStrLn . fromRockPatternString) rockPatternStrings
  print (rowSum, colSum)

testStr1 = unlines [".##..##..",
                    ".#.##.#..",
                    "#......#.",
                    "#......#.",
                    ".#.##.#..",
                    ".##..##..",
                    ".#.##.#.."]

testStr2 = unlines ["..##..##..",
                    "..#.##.#..",
                    ".#......#.",
                    ".#......#.",
                    "..#.##.#..",
                    "..##..##..",
                    "..#.##.#.."]

testStr3 = unlines ["...##..##..",
                    "...#.##.#..",
                    "..#......#.",
                    "..#......#.",
                    "...#.##.#..",
                    "...##..##..",
                    "...#.##.#.."]
