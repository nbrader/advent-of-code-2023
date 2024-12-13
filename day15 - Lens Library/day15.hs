#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package containers-0.6.7 --package split-0.2.3.5

---------------------------------
---------------------------------
----  Day 15:  Lens Library  ----
---------------------------------
---------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-21.22 ghc --package containers-0.6.7 --package split-0.2.3.5 -- -threaded -O2 '.\day15.hs'
-}

------------
-- Output --
------------
-- *Main> day15part1
-- 516657

-- *Main> day15part2
-- 210906


-------------
-- Imports --
-------------
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Char


-------------
-- Program --
-------------
main = day15part1

hash :: String -> Int
hash = go 0
  where go currentVal [] = currentVal
        go currentVal (x:xs) = go (((currentVal + ord x) * 17) `mod` 256) xs

toThisApplyAll :: a -> [a -> a] -> a
toThisApplyAll = foldl' (flip ($!))

readOpExpr :: String -> Either String (String, Int)
readOpExpr opStr
    | last opStr == '-' = Left $ takeWhile (/= '-') opStr
    | otherwise         = Right $ (\(label,'=':focalLengthStr) -> (label, read focalLengthStr)) (break (== '=') opStr)

readOp :: String -> M.Map Int [(String,Int)] -> M.Map Int [(String,Int)]
readOp opStr = case readOpExpr opStr of
    Left  label -> M.alter (\x -> case x of {Nothing -> Just []; Just xs -> Just $ filter ((/=label) . fst) xs}) (hash label)
    Right (label,focalLength) -> M.alter (\x -> case x of {Nothing -> Just [(label,focalLength)]; Just xs -> Just (case findIndex ((==label) . fst) xs of {Nothing -> xs++[(label,focalLength)]; Just i -> let (before,lens:after) = splitAt i xs in before ++ ((label,focalLength):after)})}) (hash label)

focusingPower :: M.Map Int [(String,Int)] -> Int
focusingPower m = sum [sum [product [hashInt + 1,slotNum,focalLength] | (slotNum,(label,focalLength)) <- zip [1..] lenses] | (hashInt,lenses) <- M.toList m]

day15part1 = do
  contents <- readFile "day15 (data).csv"
  let hashes = map hash . splitOn "," . filter (/= '\n') $ contents
  print . sum $ hashes

day15part2 = do
  contents <- readFile "day15 (data).csv"
  print . focusingPower . toThisApplyAll (M.empty) . map readOp . splitOn "," . filter (/= '\n') $ contents
