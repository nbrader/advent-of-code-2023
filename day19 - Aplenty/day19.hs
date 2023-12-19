#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package linear-1.22 --package split-0.2.3.5

-----------------------------------
-----------------------------------
----  Day 19: Aplenty  ----
-----------------------------------
-----------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package linear-1.22 -- '.\day19.hs' -O2
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


-------------
-- Program --
-------------
main = day19part1

data Rule = Rule deriving (Show)
data RuleTree = RuleTree deriving (Show)
data Part = Part deriving (Show)

readRules :: String -> [Rule]
readRules = map readRule . lines

readRule :: String -> Rule
readRule = undefined

toRuleTree :: [Rule] -> RuleTree
toRuleTree = undefined

readParts :: String -> [Part]
readParts = map readPart . lines

readPart :: String -> Part
readPart = undefined

reduce :: RuleTree -> RuleTree
reduce = undefined

apply :: RuleTree -> Part -> Maybe Part
apply = undefined

partScore :: Part -> Int
partScore = undefined

day19part1 = do
  contents <- readFile "day19 (example).csv"
  let [rulesStr, partsStr] = splitOn "\n\n" $ contents
  let rules = readRules rulesStr
  let ruleTree = toRuleTree rules
  let parts = readParts $ partsStr
  let reducedRuleTree = reduce ruleTree
  let acceptedParts = catMaybes . map (apply reducedRuleTree) $ parts
  let total = sum $ map partScore acceptedParts
  print total
