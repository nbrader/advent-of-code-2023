#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22 --package fastmemo-0.1.1

-------------------------------
-------------------------------
----  Day 7:  Camel Cards  ----
-------------------------------
-------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package split-0.2.3.5 --package containers-0.6.7 --package linear-1.22 -- '.\day7.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day7part1
-- 

-- *Main> day7part2
-- 


-------------
-- Imports --
-------------
import Data.Char (isDigit)
import Data.List (sort, tails, isPrefixOf, groupBy, find, nub, foldl', group, sort)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)
import Data.Tuple (swap)
import Data.Function (on, fix)
import Data.Function.FastMemo (memoize)
import Data.Ord (comparing)


-------------
-- Program --
-------------
main = day7part1

data Hand = Hand {handCards :: String, handBid :: Integer} deriving (Show, Eq)
data Type = HighCard
          | OnePair
          | TwoPair
          | ThreeOfAKind
          | FullHouse
          | FourOfAKind
          | FiveOfAKind deriving (Show, Ord, Eq)

cardValue '2' = 1
cardValue '3' = 2
cardValue '4' = 3
cardValue '5' = 4
cardValue '6' = 5
cardValue '7' = 6
cardValue '8' = 7
cardValue '9' = 8
cardValue 'T' = 9
cardValue 'J' = 10
cardValue 'Q' = 11
cardValue 'K' = 12
cardValue 'A' = 13

handType :: Hand -> Type
handType hand = if 5 `elem` numbers
                  then FiveOfAKind
                  else if 4 `elem` numbers
                        then FourOfAKind
                        else if 3 `elem` numbers && 2 `elem` numbers
                              then FullHouse
                              else if 3 `elem` numbers
                                    then ThreeOfAKind
                                    else if (== 2) . length $ filter (== 2) numbers
                                          then TwoPair
                                          else if 3 `elem` numbers
                                                then OnePair
                                                else HighCard
  where numbers = map length . group . sort . handCards $ hand

instance Ord Hand where
  compare x y = comparing handType x y <> comparing (map cardValue . handCards) x y

readHand :: String -> Hand
readHand inStr = (\[cards, bid] -> Hand cards (read bid)) $ words inStr

day7part1 = do
  contents <- readFile "day7 (data).csv"
  print $ sum . map (\(i,h) -> i * handBid h) . zip [1..] . sort . map readHand . lines $ contents

-- day7part2 = do
  -- contents <- readFile "day7 (data).csv"
  -- print $ numberOfWinningOptions' $ readAsSingleRace $ contents