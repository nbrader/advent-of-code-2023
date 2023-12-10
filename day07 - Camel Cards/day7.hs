#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci

-------------------------------
-------------------------------
----  Day 7:  Camel Cards  ----
-------------------------------
-------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc -- '.\day7.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day7part1
-- 251216224

-- *Main> day7part2
-- 250825971


-------------
-- Imports --
-------------
import Data.List (sort, group)
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

cardWithJokersValue 'J' = 1
cardWithJokersValue '2' = 2
cardWithJokersValue '3' = 3
cardWithJokersValue '4' = 4
cardWithJokersValue '5' = 5
cardWithJokersValue '6' = 6
cardWithJokersValue '7' = 7
cardWithJokersValue '8' = 8
cardWithJokersValue '9' = 9
cardWithJokersValue 'T' = 10
cardWithJokersValue 'Q' = 11
cardWithJokersValue 'K' = 12
cardWithJokersValue 'A' = 13

allCards = "23456789TJQKA"

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
                                          else if 2 `elem` numbers
                                                then OnePair
                                                else HighCard
  where numbers = map length . group . sort . handCards $ hand

newtype HandWithJokers = HandWithJokers {fromHandWithJokers :: Hand} deriving (Show, Eq)

handWithJokersCards = handCards . fromHandWithJokers
handWithJokersBid = handBid . fromHandWithJokers

bestHandWithJokersHandType :: HandWithJokers -> Type
bestHandWithJokersHandType (HandWithJokers h) = maximum $ map handType hands
  where hands = map (\cs -> Hand cs (handBid h)) $ sequence [if c == 'J' then allCards else [c] | c <- handCards h]

instance Ord Hand where
  compare x y = comparing handType x y <> comparing (map cardValue . handCards) x y
  
instance Ord HandWithJokers where
  compare x y = comparing bestHandWithJokersHandType x y <> comparing (map cardWithJokersValue . handWithJokersCards) x y
-- I might have to avoid using Ord to allow me to cache the map from joker hand to best hand

readHand :: String -> Hand
readHand inStr = (\[cards, bid] -> Hand cards (read bid)) $ words inStr

readHandWithJokers :: String -> HandWithJokers
readHandWithJokers = HandWithJokers . readHand

day7part1 = do
  contents <- readFile "day7 (data).csv"
  print $ sum . map (\(i,h) -> i * handBid h) . zip [1..] . sort . map readHand . lines $ contents

day7part2 = do
  contents <- readFile "day7 (data).csv"
  print $ sum . map (\(i,h) -> i * handWithJokersBid h) . zip [1..] . sort . map readHandWithJokers . lines $ contents