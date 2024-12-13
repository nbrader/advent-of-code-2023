#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5

---------------------------
---------------------------
----  Day 19: Aplenty  ----
---------------------------
---------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 -- '.\day19.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day19part1
-- 

-- *Main> day19part2
-- 


-------------
-- Imports --
-------------
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust)


-------------
-- Program --
-------------
main = day19part1

data Category = X | M | A | S deriving (Show, Eq)
data Inequality = NoConstraint | LessThan Category Int | GreaterThan Category Int deriving (Show, Eq)
data Rule = Rule {ruleName :: String, ruleInequalitiesAndResults :: [(Inequality,String)]} deriving (Show)
data RuleTree = RuleTree {ruleTreeInequality :: Inequality, ruleTreeSatisfiedNext :: Predicate, ruleTreeNotSatisfiedNext :: Predicate} deriving (Show, Eq)
data Predicate = RuleTreeResult RuleTree | Accept | Reject deriving (Show, Eq)
data Part = Part {partX :: Int, partM :: Int, partA :: Int, partS :: Int} deriving (Show)

readRules :: String -> [Rule]
readRules = map readRule . lines

readCategory :: String -> Category
readCategory "x" = X
readCategory "m" = M
readCategory "a" = A
readCategory "s" = S
readCategory  c  = error c

readInequalityAndSatisfiedResult :: String -> (Inequality,String)
readInequalityAndSatisfiedResult inStr
    | '<' `elem` inStr = (\[catStr,boundaryAndResultStr] -> let [boundaryStr,resultStr] = splitOn ":" boundaryAndResultStr in (LessThan    (readCategory catStr) (read boundaryStr), resultStr)) . splitOn "<" $ inStr
    | '>' `elem` inStr = (\[catStr,boundaryAndResultStr] -> let [boundaryStr,resultStr] = splitOn ":" boundaryAndResultStr in (GreaterThan (readCategory catStr) (read boundaryStr), resultStr)) . splitOn ">" $ inStr
    | otherwise        = (NoConstraint, inStr)

readInequalitiesAndResults :: String -> [(Inequality,String)]
readInequalitiesAndResults = map readInequalityAndSatisfiedResult . splitOn ","

inequalitiesAndResultsToRuleTree :: M.Map String Rule -> [(Inequality,String)] -> RuleTree
inequalitiesAndResultsToRuleTree rulesMap [(NoConstraint,"R")] = RuleTree NoConstraint Reject Accept
inequalitiesAndResultsToRuleTree rulesMap [(NoConstraint,"A")] = RuleTree NoConstraint Accept Accept
inequalitiesAndResultsToRuleTree rulesMap [(NoConstraint,resultStr)] = RuleTree NoConstraint (RuleTreeResult (let (Rule _ inequalitiesAndResults) = fromJust $ M.lookup resultStr rulesMap in inequalitiesAndResultsToRuleTree rulesMap inequalitiesAndResults)) Accept
inequalitiesAndResultsToRuleTree rulesMap ((inequality,"R"):inequalitiesAndResults) = RuleTree inequality Reject (RuleTreeResult $ inequalitiesAndResultsToRuleTree rulesMap inequalitiesAndResults)
inequalitiesAndResultsToRuleTree rulesMap ((inequality,"A"):inequalitiesAndResults) = RuleTree inequality Accept (RuleTreeResult $ inequalitiesAndResultsToRuleTree rulesMap inequalitiesAndResults)
inequalitiesAndResultsToRuleTree rulesMap ((inequality,resultStr):inequalitiesAndResults) = RuleTree inequality (RuleTreeResult (let (Rule _ inequalitiesAndResults) = fromJust $ M.lookup resultStr rulesMap in inequalitiesAndResultsToRuleTree rulesMap inequalitiesAndResults)) (RuleTreeResult $ inequalitiesAndResultsToRuleTree rulesMap inequalitiesAndResults)

readRule :: String -> Rule
readRule inStr = Rule { ruleName = nameStr,
                        ruleInequalitiesAndResults = readInequalitiesAndResults inequalityStr }
  where (nameStr, after1) = break (=='{') inStr
        (inequalityStr, _) = break (=='}') (drop (length "{") $ after1)

rulesToRuleMap :: [Rule] -> M.Map String Rule
rulesToRuleMap rules = M.fromList [(ruleName r, r) | r <- rules]

toRuleTree :: [Rule] -> RuleTree
toRuleTree rules = inequalitiesAndResultsToRuleTree rulesMap inRuleInequalitiesAndResults
  where rulesMap = rulesToRuleMap rules
        Just (Rule "in" inRuleInequalitiesAndResults) = M.lookup "in" rulesMap

readParts :: String -> [Part]
readParts = map readPart . lines

readPart :: String -> Part
readPart inStr = Part x m a s
  where (xStr, after1) = break (==',') (drop (length "{x=") $ inStr)
        (mStr, after2) = break (==',') (drop (length ",m=") $ after1)
        (aStr, after3) = break (==',') (drop (length ",a=") $ after2)
        (sStr, _) = break (=='}') (drop (length ",s=") $ after3)
        
        x = read xStr
        m = read mStr
        a = read aStr
        s = read sStr

reduce :: Predicate -> Predicate
reduce (RuleTreeResult (RuleTree  NoConstraint                   satisfied notSatisfied)) = reduce satisfied
reduce (RuleTreeResult (RuleTree (GreaterThan category boundary) satisfied notSatisfied)) = reduce $ RuleTreeResult (RuleTree (LessThan category (boundary+1)) notSatisfied satisfied)
reduce (RuleTreeResult (RuleTree (LessThan category1 boundary1)  satisfied@(RuleTreeResult (RuleTree (LessThan category2 boundary2) innerSatisfied _)) notSatisfied))
    -- | category1 == category2 && boundary1 > boundary2 = reduce innerSatisfied
    | ( reducedSatisfied /= satisfied
      || reducedNotSatisfied /= notSatisfied)         = RuleTreeResult (RuleTree (LessThan category1 boundary1) reducedSatisfied reducedNotSatisfied)
    | otherwise                                       = RuleTreeResult (RuleTree (LessThan category1 boundary1) satisfied        notSatisfied)
  where reducedSatisfied    = reduce satisfied
        reducedNotSatisfied = reduce notSatisfied
reduce (RuleTreeResult (RuleTree (LessThan category boundary) satisfied notSatisfied)) = RuleTreeResult (RuleTree (LessThan category boundary) (reduce satisfied) (reduce notSatisfied))
reduce Accept = Accept
reduce Reject = Reject

data Interval = Interval {intervalLower :: Maybe Int, intervalUpper :: Maybe Int} deriving (Show)
data Bounds = Bounds {boundsX :: Interval, boundsM :: Interval, boundsA :: Interval, boundsS :: Interval} deriving (Show)
emptyBounds = Bounds (Interval Nothing Nothing) (Interval Nothing Nothing) (Interval Nothing Nothing) (Interval Nothing Nothing)

boundsVolume :: Bounds -> Int
boundsVolume (Bounds (Interval (Just xMin) (Just xMax)) (Interval (Just mMin) (Just mMax)) (Interval (Just aMin) (Just aMax)) (Interval (Just sMin) (Just sMax))) = product [xSize, mSize, aSize, sSize]
  where xSize = clampPositive $ xMax - xMin + 1
        mSize = clampPositive $ mMax - mMin + 1
        aSize = clampPositive $ aMax - aMin + 1
        sSize = clampPositive $ sMax - sMin + 1
boundsVolume (Bounds xInterval mInterval aInterval sInterval) = product [intervalSizeInBoundsBodge xInterval, intervalSizeInBoundsBodge mInterval, intervalSizeInBoundsBodge aInterval, intervalSizeInBoundsBodge sInterval]

intervalSizeInBoundsBodge :: Interval -> Int
intervalSizeInBoundsBodge (Interval Nothing Nothing) = 4001 - 1 + 1
intervalSizeInBoundsBodge (Interval Nothing (Just upper)) = clampPositive (min upper 4001) - 1 + 1
intervalSizeInBoundsBodge (Interval (Just lower) Nothing) = 4001 - max lower 1 + 1
intervalSizeInBoundsBodge (Interval (Just lower) (Just upper)) = clampPositive (min upper 4001) - max lower 1 + 1

clampPositive x
    | x < 0     = 0
    | otherwise = x

trimIntervalToAbove :: Int -> Interval -> Interval
trimIntervalToAbove boundary (Interval Nothing      Nothing     ) = Interval (Just $ boundary+1) Nothing
trimIntervalToAbove boundary (Interval Nothing      (Just upper)) = Interval (Just $ boundary+1) (Just upper)
trimIntervalToAbove boundary (Interval (Just lower) Nothing     )
    | boundary < lower = Interval (Just lower) Nothing
    | otherwise        = Interval (Just $ boundary+1) Nothing
trimIntervalToAbove boundary (Interval (Just lower) (Just upper))
    | boundary < lower = Interval (Just lower) (Just upper)
    | otherwise        = Interval (Just $ boundary+1) (Just upper)

trimIntervalToBelow :: Int -> Interval -> Interval
trimIntervalToBelow boundary (Interval Nothing      Nothing) = Interval Nothing      (Just $ boundary-1)
trimIntervalToBelow boundary (Interval (Just lower) Nothing) = Interval (Just lower) (Just $ boundary-1)
trimIntervalToBelow boundary (Interval Nothing (Just upper))
    | boundary > upper = Interval Nothing      (Just upper)
    | otherwise        = Interval Nothing      (Just $ boundary-1)
trimIntervalToBelow boundary (Interval (Just lower) (Just upper))
    | boundary > upper = Interval (Just lower) (Just upper)
    | otherwise        = Interval (Just lower) (Just $ boundary-1)

trimBoundsToAbove :: Int -> Category -> Bounds -> Bounds
trimBoundsToAbove boundary X bounds = bounds {boundsX = trimIntervalToAbove boundary (boundsX bounds)}
trimBoundsToAbove boundary M bounds = bounds {boundsM = trimIntervalToAbove boundary (boundsM bounds)}
trimBoundsToAbove boundary A bounds = bounds {boundsA = trimIntervalToAbove boundary (boundsA bounds)}
trimBoundsToAbove boundary S bounds = bounds {boundsS = trimIntervalToAbove boundary (boundsS bounds)}

trimBoundsToBelow :: Int -> Category -> Bounds -> Bounds
trimBoundsToBelow boundary X bounds = bounds {boundsX = trimIntervalToBelow boundary (boundsX bounds)}
trimBoundsToBelow boundary M bounds = bounds {boundsM = trimIntervalToBelow boundary (boundsM bounds)}
trimBoundsToBelow boundary A bounds = bounds {boundsA = trimIntervalToBelow boundary (boundsA bounds)}
trimBoundsToBelow boundary S bounds = bounds {boundsS = trimIntervalToBelow boundary (boundsS bounds)}

volume :: Predicate -> Int
volume pred = go (emptyBounds, pred)
  where go :: (Bounds, Predicate) -> Int
        go (bounds, RuleTreeResult (RuleTree (LessThan category boundary) satisfied notSatisfied)) = go (trimBoundsToBelow boundary category bounds, satisfied) + go (trimBoundsToAbove boundary category bounds, notSatisfied)
        go (bounds, Accept) = boundsVolume bounds
        go (_, Reject) = 0
        go x = error (show x)

satisfies :: Part -> Predicate -> Bool
(Part x m a s) `satisfies` (RuleTreeResult (RuleTree NoConstraint             satisfied notSatisfied)) = Part x m a s `satisfies` satisfied
(Part x m a s) `satisfies` (RuleTreeResult (RuleTree (LessThan    X boundary) satisfied notSatisfied)) = if x < boundary then Part x m a s `satisfies` satisfied else Part x m a s `satisfies` notSatisfied
(Part x m a s) `satisfies` (RuleTreeResult (RuleTree (LessThan    M boundary) satisfied notSatisfied)) = if m < boundary then Part x m a s `satisfies` satisfied else Part x m a s `satisfies` notSatisfied
(Part x m a s) `satisfies` (RuleTreeResult (RuleTree (LessThan    A boundary) satisfied notSatisfied)) = if a < boundary then Part x m a s `satisfies` satisfied else Part x m a s `satisfies` notSatisfied
(Part x m a s) `satisfies` (RuleTreeResult (RuleTree (LessThan    S boundary) satisfied notSatisfied)) = if s < boundary then Part x m a s `satisfies` satisfied else Part x m a s `satisfies` notSatisfied
(Part x m a s) `satisfies` (RuleTreeResult (RuleTree (GreaterThan X boundary) satisfied notSatisfied)) = if x > boundary then Part x m a s `satisfies` satisfied else Part x m a s `satisfies` notSatisfied
(Part x m a s) `satisfies` (RuleTreeResult (RuleTree (GreaterThan M boundary) satisfied notSatisfied)) = if m > boundary then Part x m a s `satisfies` satisfied else Part x m a s `satisfies` notSatisfied
(Part x m a s) `satisfies` (RuleTreeResult (RuleTree (GreaterThan A boundary) satisfied notSatisfied)) = if a > boundary then Part x m a s `satisfies` satisfied else Part x m a s `satisfies` notSatisfied
(Part x m a s) `satisfies` (RuleTreeResult (RuleTree (GreaterThan S boundary) satisfied notSatisfied)) = if s > boundary then Part x m a s `satisfies` satisfied else Part x m a s `satisfies` notSatisfied
(Part x m a s) `satisfies` Reject = False
(Part x m a s) `satisfies` Accept = True

partScore :: Part -> Int
partScore (Part x m a s) = sum [x, m, a, s]

allParts = [Part x m a s | let range = [1..4000], x <- range, m <- range, a <- range, s <- range]

addBoundBox :: Predicate -> Predicate
addBoundBox = applyLowers . applyUppers
  where minVal = 1
        maxVal = 4000
        lowerBound = minVal-1
        upperBound = maxVal+1
        
        applyLowers pred = RuleTreeResult $ RuleTree (GreaterThan X lowerBound) (RuleTreeResult $ RuleTree (GreaterThan M lowerBound) (RuleTreeResult $ RuleTree (GreaterThan A lowerBound) (RuleTreeResult $ RuleTree (GreaterThan S lowerBound) pred Reject) Reject) Reject) Reject
        applyUppers pred = RuleTreeResult $ RuleTree (LessThan    X upperBound) (RuleTreeResult $ RuleTree (LessThan    M upperBound) (RuleTreeResult $ RuleTree (LessThan    A upperBound) (RuleTreeResult $ RuleTree (LessThan    S upperBound) pred Reject) Reject) Reject) Reject

day19part1 = do
  contents <- readFile "day19 (data).csv"
  let [rulesStr, partsStr] = splitOn "\n\n" $ contents
  let rules = readRules rulesStr
  let pred = RuleTreeResult $ toRuleTree rules
  let parts = readParts $ partsStr
  let acceptedParts = filter (`satisfies` pred) $ parts
  let total = sum $ map partScore acceptedParts
  print total

day19part2 = do
  contents <- readFile "day19 (example).csv"
  let [rulesStr, partsStr] = splitOn "\n\n" $ contents
  let rules = readRules rulesStr
  let pred = RuleTreeResult $ toRuleTree rules
  let predWithBounds = addBoundBox pred
  let reducedPred = reduce predWithBounds
  let total = volume reducedPred
  print total