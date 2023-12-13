#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22 --package split-0.2.3.5 --package parallel-3.2.2.0

--------------------------------
--------------------------------
----  Day 12:  Hot Springs  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package linear-1.22 -- -threaded -O2 '.\day12.hs'
-}

------------
-- Output --
------------
-- *Main> day12part1
-- 

-- *Main> day12part2
-- 


-------------
-- Imports --
-------------
import Data.List (foldl', nub, sort, transpose, isPrefixOf, findIndex, intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybeToList, catMaybes)
import Linear.V2
import Control.Monad (guard)
import Debug.Trace (trace)
import Control.Parallel.Strategies


-------------
-- Program --
-------------
main = day12part2

data SpringRow = SpringRow {srConditionsStr :: String, srDamagedRuns :: [Int]} deriving (Show)

readSpringRow :: String -> SpringRow
readSpringRow = (\[conditionsStr,groupsStr] -> SpringRow conditionsStr (map read $ splitOn "," groupsStr)) . words

ignore _ x = x

arrangements :: SpringRow -> [String]
arrangements sr@(SpringRow ""            []                      ) = [""]
arrangements sr@(SpringRow ""            (damagedRun:damagedRuns)) = []
arrangements sr@(SpringRow conditionsStr [])
    | all (\c -> c == '.' || c == '?') conditionsStr = [allAsUndamaged]
    | otherwise                                      = []
  where allAsUndamaged = replicate (length conditionsStr) '.'
arrangements sr@(SpringRow conditionsStr (damagedRun:damagedRuns))
    |    enoughCharsToHaveDamagedRun && matchesDamagedRun
      && anyFollowingCharIsNonDamaged = (map ((damagedPrefix ++ followingCharStr) ++) . arrangements $ (SpringRow (drop (damagedRun + length followingCharStr) conditionsStr) damagedRuns)) ++ arrangementsStartingWithUndamaged
    | otherwise                       = arrangementsStartingWithUndamaged
  where damagedPrefix = replicate damagedRun '#'
        enoughCharsToHaveDamagedRun = length conditionsStr >= damagedRun
        matchesDamagedRun = all (\c -> c == '#' || c == '?') $ take damagedRun conditionsStr
        anyFollowingCharIsNonDamaged = (\xs -> case xs of {[] -> True; (x':_) -> x' /= '#'}) . drop damagedRun $ conditionsStr
        followingCharStr = map (const '.') . (\xs -> case xs of {[] -> ""; (x':_) -> [x']}) . drop damagedRun $ conditionsStr
        headUndamaged = (\c -> c == '.' || c == '?') . head $ conditionsStr
        arrangementsStartingWithUndamaged
            | headUndamaged = map ('.':) . arrangements $ (SpringRow (tail conditionsStr) (damagedRun:damagedRuns))
            | otherwise     = []

foldedArrangements :: Int -> SpringRow -> [String]
foldedArrangements dupeCount sr@(SpringRow conditionsStr damagedRuns)
    = case after of
        [] -> naiveFoldedArrangements dupeCount (SpringRow conditionsStr damagedRuns)
        _ -> case before of
            [] -> naiveFoldedArrangements dupeCount (SpringRow conditionsStr damagedRuns) -- map concat $ sequence $ (replicate (dupeCount-1) (arrangements (SpringRow (conditionsStr ++ "?") damagedRuns)) ++ [arrangements (SpringRow conditionsStr damagedRuns)])
            _ -> concat $ do
                    numOfInitRuns <- [0 .. (length damagedRuns)]
                    let (initRuns,finalRuns) = splitAt numOfInitRuns damagedRuns
                    
                    return $ map concat $ sequence $ concat [
                                    [arrangements (SpringRow before initRuns)],
                                    replicate (dupeCount-1) (arrangements (SpringRow (concat [after, '?':before]) (concat [finalRuns, initRuns]))),
                                    [arrangements (SpringRow after finalRuns)]
                                    ]
  where (before,after) = break (== '.') conditionsStr

numOfFoldedArrangements :: Int -> SpringRow -> Int
numOfFoldedArrangements dupeCount sr@(SpringRow conditionsStr damagedRuns)
    = case after of
        [] -> length $ naiveFoldedArrangements dupeCount (SpringRow conditionsStr damagedRuns)
        _ -> case before of 
            [] -> length $ naiveFoldedArrangements dupeCount (SpringRow conditionsStr damagedRuns) -- (*dupeCount) $ length (arrangements (SpringRow (conditionsStr ++ "?") damagedRuns))
            _ -> sum $ do
                numOfInitRuns <- [0 .. (length damagedRuns)]
                let (initRuns,finalRuns) = splitAt numOfInitRuns damagedRuns
                
                return $ product [
                                     length $ arrangements (SpringRow before initRuns),
                                    (length $ arrangements (SpringRow (concat [after, '?':before]) (concat [finalRuns, initRuns]))) ^ (dupeCount-1),
                                     length $ arrangements (SpringRow after finalRuns)
                                ]
  where (before,after) = break (== '.') conditionsStr

naiveFoldedArrangements dupeCount (SpringRow conditionsStr damagedRuns) = arrangements (SpringRow (intercalate "?" $ replicate dupeCount conditionsStr) (concat $ replicate dupeCount damagedRuns))

day12part1 = do
  contents <- readFile "day12 (data).csv"
  print . sum . map length . map arrangements . map readSpringRow . lines $ contents

day12part2 = do
  contents <- readFile "day12 (data).csv"
  let numsOfArrangements = map (numOfFoldedArrangements 5) . take 325 . drop 486 . map readSpringRow . lines $ contents
  let numsOfArrangementsInParallel = numsOfArrangements `using` parList rdeepseq
  mapM_ print numsOfArrangementsInParallel

-- day12part2 = do
  -- contents <- readFile "day12 (example).csv"
  
  -- mapM_ print . (foldedArrangements 5) . (!! 1) . map readSpringRow . lines $ contents
  
  
-- 6444
-- 1
-- 1024
-- 7203
-- 32
-- 1
-- 9604
-- 32
-- 32
-- 10068540
-- 162
-- 162
-- 556875
-- 7776
-- 124416
-- 268912
-- 589454
-- 32
-- 668168
-- 1
-- 162
-- 33
-- 32
-- 182331
-- 28464
-- 32
-- 16
-- 42386415
-- 94026576
-- 16
-- 776027
-- 74690704
-- 1
-- 61296
-- 75250
-- 32
-- 1280000
-- 189399
-- 5184
-- 4834
-- 768
-- 1
-- 256
-- 944784
-- 2592
-- 243
-- 28315
-- 1875
-- 5184
-- 553580718
-- 16
-- 1875
-- 1024
-- 243
-- 1
-- 162
-- 3125
-- 32
-- 2618986
-- 243
-- 18683879
-- 4949161
-- 3904
-- 16487732
-- 3125
-- 512
-- 243
-- 324
-- 1024
-- 1250
-- 5184
-- 7812500
-- 16
-- 16
-- 512
-- 13225984
-- 16
-- 54868
-- 163
-- 230496
-- 1875
-- 24055072
-- 16807
-- 5184
-- 2592
-- 256
-- 1
-- 162
-- 486
-- 186624
-- 2500
-- 1
-- 32
-- 32
-- 59049
-- 1049760
-- 248832
-- 32
-- 7092560
-- 8192
-- 1250
-- 38631168
-- 2500
-- 2300
-- 2437
-- 16384
-- 32
-- 171366
-- 1393
-- 8819148
-- 9765625
-- 2528253
-- 10354598
-- 745
-- 24991
-- 349632
-- 7204
-- 12606976
-- 1
-- 14406
-- 4050000
-- 1993
-- 3888
-- 1
-- 15727842
-- 32
-- 32
-- 162
-- 1
-- 16384
-- 100930443
-- 3125
-- 243
-- 32
-- 5184
-- 1
-- 1857184
-- 32768
-- 16
-- 5184
-- 162
-- 28478911
-- 5184
-- 1048576
-- 1920000
-- 15757092282
-- 39367
-- 1
-- 47029248
-- 1
-- 9470469
-- 16
-- 70400
-- 32
-- 62208
-- 1
-- 7084
-- 32
-- 1024
-- 2500
-- 16
-- 922752
-- 1250
-- 32
-- 1889568
-- 1539
-- 786432
-- 544
-- 7776
-- 7776
-- 1539
-- 2819664
-- 1
-- 9604
-- 3888
-- 9604
-- 162
-- 32
-- 607500
-- 70000
-- 16
-- 16
-- 1
-- 3125
-- 1
-- 1250
-- 3981312
-- 162
-- 16282944
-- 16807
-- 32
-- 32
-- 3454
-- 52488
-- 3699
-- 16
-- 396
-- 5184
-- 151875
-- 10126172
-- 162
-- 32
-- 5184
-- 1250
-- 102487
-- 32
-- 1
-- 31083627
-- 3211452
-- 667591
-- 3363
-- 1024
-- 7690801
-- 3658430
-- 22005
-- 81
-- 20971520
-- 720896
-- 384160
-- 243
-- 39366
-- 1
-- 5427
-- 768
-- 17995768
-- 6444
-- 162
-- 32805
-- 24576
-- 243
-- 179361696
-- 87846
-- 14406
-- 30045015
-- 512
-- 1166886
-- 103680
-- 1
-- 3384
-- 7203
-- 16
-- 16384
-- 39366
-- 32
-- 27588
-- 163
-- 162
-- 744310000
-- 100000
-- 1433531
-- 2500
-- 32
-- 1
-- 1
-- 243
-- 60000
-- 512
-- 30096021
-- 4278353625
-- 499
-- 2538
-- 162
-- 50607936
-- 7776
-- 1250
-- 3888
-- 627883
-- 768
-- 5184
-- 5859375
-- 1956
-- 162
-- 655360
-- 90000
-- 958691
-- 243
-- 1
-- 8615945
-- 111936400
-- 2500
-- 16
-- 32
-- 124416
-- 512
-- 944784
-- 456490
-- 230496
-- 7776
-- 2500
-- 41472
-- 16
-- 768
-- 243
-- 243
-- 124416
-- 1
-- 32
-- 768
-- 602376
-- 177723
-- 117128
-- 3200000
-- 171366
-- 32
-- 7776
-- 2089
-- 32
-- 40000
-- 243
-- 324
-- 48582261
-- 16384
-- 16
-- 5184
-- 1024
-- 1
-- 1250
-- 1
-- 32
-- 14376116
-- 76832
-- 5184
-- 81
-- 745433
-- 243
-- 16
-- 1
-- 1024
-- 1875
-- 100243282
-- 52488
-- 10946
-- 9604
-- 5184
-- 2002
-- 256
-- 221031
-- 1
-- 12005
-- 243
-- 1105
-- 124416
-- 2500
-- 512
-- 32
-- 100000
-- 32
-- 162
-- 7344705
-- 81
-- 243
-- 4299
-- 4428111
-- 256
-- 32
-- 4993
-- 1
-- 2500
-- 59049
-- 1
-- 1036995
-- 1
-- 1250
-- 5184
-- 5184
-- 32768
-- 260236
-- 32
-- 1024
-- 1
-- 161051
-- 81947069
-- 2500
-- 237766753
-- 1
-- 1338
-- 162
-- 1
-- 32
-- 26244
-- 16
-- 81
-- 162
-- 248832
-- 1024
-- 395191
-- 31356
-- 7203
-- 512
-- 960000
-- 124416
-- 786432
-- 256
-- 5184
-- 243
-- 183579396
-- 243
-- 6121472
-- 512
-- 2592
-- 1024
-- 162
-- 42476461
-- 83205
-- 1
-- 29283
-- 8192
-- 80000
-- 60000
-- 1
-- 121392438
-- 243
-- 768
-- 13363360
-- 512
-- 32
-- 371436
-- 1280837
-- 417605
-- 1024
-- 16
-- 768
-- 512
-- 871289333
-- 162
-- 10368
-- 32
-- 162
-- 712
-- 1875
-- 37515625
-- 777600000
-- 162
-- 1345
-- 629856
-- 176802
-- 634451
-- 139961
-- 249248
-- 20565
-- 1
-- 2500
-- 1
-- 32
-- 1408212
-- 70000
-- 7203
-- 243
-- 4131
-- 3888
-- 162
-- 5184
-- 12005
-- 81
-- 1112336
-- 4964
-- 7203
-- 584647
-- 5184
-- 1
-- 512
-- 15816250
-- 171366
-- 12005
-- 6256515
-- 1024
-- 165888
-- 32
-- 45943
-- 1024
-- 16
-- 460992
-- 16
-- 14406
-- 3888
-- 455625
-- 33013750
-- 280593
-- 9020
-- 768
-- 1574640
-- 32
-- 5184
-- 589824
-- 125951
-- 7952167118
-- 5741

-- ...

-- 362
-- 544
-- 32805
-- 2500
-- 60000
-- 56816933
-- 16
-- 243
-- 3982352
-- 34
-- 5184
-- 32
-- 52521875
-- 3167984
-- 744310000
-- 291648
-- 2581
-- 16
-- 1
-- 32
-- 32
-- 6444
-- 458752
-- 32
-- 1
-- 303750
-- 11881376
-- 307328
-- 16384
-- 16
-- 24576
-- 2500
-- 2333772
-- 162
-- 16384
-- 810133
-- 14406
-- 1024
-- 1048576
-- 147035
-- 87846
-- 512
-- 162
-- 243
-- 32
-- 14568
-- 2328612
-- 1956
-- 1962
-- 1
-- 7777
-- 32
-- 5184
-- 59683
-- 132947
-- 1
-- 32
-- 512
-- 2662
-- 1024
-- 458752
-- 158339
-- 1
-- 720896
-- 171135
-- 110985
-- 17663197
-- 32
-- 17911
-- 256
-- 584647
-- 2592
-- 3125
-- 345744
-- 32
-- 1024
-- 32
-- 197
-- 2500
-- 15620
-- 1
-- 32
-- 142149424
-- 162
-- 32
-- 32768
-- 124416
-- 2500
-- 87846
-- 32
-- 3120685
-- 2809716798
-- 146410
-- 1024
-- 1024
-- 512
-- 26244
-- 243
-- 1875
-- 1024
-- 1
-- 1250
-- 1
-- 768
-- 4050
-- 3888
-- 32768
-- 1
-- 32
-- 30012500
-- 32
-- 60256
-- 1
-- 162
-- 145152
-- 1024
-- 1
-- 2825
-- 540162
-- 243
-- 786464
-- 47849280
-- 1493
-- 165888
-- 16
-- 32
-- 1024
-- 184620512
-- 14406
-- 1
-- 344207360
-- 7776
-- 165888
-- 292144
-- 329422500
-- 3888
-- 768
-- 1
-- 81
-- 1
-- 32
-- 162
-- 162
-- 32
-- 512
-- 32
-- 43091446
-- 455625
-- 2990912
-- 162
-- 177247
-- 3888
-- 243
-- 146410
-- 3032677
-- 2344990116
-- 533
-- 1
-- 162
-- 35871875
-- 1296
-- 1172889
-- 9604
-- 124416
-- 162
-- 512
-- 63876
-- 32805
-- 243
-- 32
-- 243
-- 1313210
-- 74808
-- 243
-- 1
-- 1024
-- 243
-- 14833458
-- 316098
-- 3192324
-- 194
-- 7203
-- 512
-- 96108
-- 170438
-- 1875
-- 1600256
-- 243
-- 24576
-- 1
-- 243
-- 256
-- 16
-- 16807
-- 243
-- 1
-- 5184
-- 124416
-- 455881
-- 32
-- 32
-- 16384
-- 243
-- 16
-- 243
-- 124416
-- 32
-- 186625
-- 32
-- 81
-- 32
-- 912380000
-- 1024
-- 32768
-- 7776
-- 1
-- 80000
-- 622227
-- 3387696
-- 32
-- 1875
-- 34153761
-- 153
-- 512
-- 131072
-- 1
-- 769
-- 5184
-- 3906875
-- 243
-- 16807
-- 7776
-- 32
-- 1024
-- 24727
-- 243
-- 244
-- 32
-- 1024
-- 256
-- 1
-- 21240983
-- 16384
-- 607500
-- 1
-- 392
-- 14406
-- 32
-- 192080
-- 81
-- 16384
-- 7776
-- 32
-- 92824
-- 1
-- 39016250
-- 36015000
-- 32
-- 768
-- 178176
-- 7203
-- 87846
-- 16
-- 243
-- 26244
-- 51078843
-- 512
-- 32
-- 162
-- 230154
-- 512
-- 2500
-- 16
-- 16
-- 202417180
-- 14406
-- 32
-- 32
-- 514442
-- 32768
-- 571
-- 629856
-- 32
-- 3195232
-- 215408
-- 32
-- 142805
-- 162
-- 1602401
-- 256
-- 5184
-- 6480
-- 60000
-- 2968
-- 768
-- 16807
-- 243
-- 134735
-- 243
-- 8192
-- 651563391
-- 1024
-- 14406
-- 512
-- 262144
-- 19683
-- 32
-- 16
-- 111872
-- 768
-- 15184