#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package QuickCheck-2.14.3

module Util where

import Control.Applicative

enumPairUnsigned :: (Int,Int) -> Int
enumPairUnsigned (x,y) = ((x+y+1)*(x+y) `div` 2) + y

-- enumPairUnsigned :: (Int,Int) -> Int
-- enumPairUnsigned (x,y)
    -- | (x+y) `mod` 2 == 0 = ((x+y+1)*(x+y) `div` 2) + y
    -- | otherwise          = ((x+y+1)*(x+y) `div` 2) + x

-- enumPairUnsignedInv :: Int -> (Int,Int)
enumInv n = w
  where n' = fromInteger . toInteger $ n
        w = floor ((sqrt (8*n'+1) - 1)/2)

-- enumPairUnsignedInv :: Int -> (Int,Int)
-- enumPairUnsignedInv n = if p then (x,y) else (y,x)
  -- where n' = fromInteger . toInteger $ n
        -- w = floor ((sqrt (8*n'+1) - 1)/2)
        -- t = (w^2+w) `div` 2
        -- y = n-t
        -- x = w-y
        -- p = (x+y) `mod` 2 == 0

-- enumPairSigned :: (Int,Int) -> Int
-- enumPairSigned (x,y) = enumPairUnsigned (enumSigned x, enumSigned y)

-- enumPairSignedInv :: Int -> (Int,Int)
-- enumPairSignedInv n = (enumSignedInv x, enumSignedInv y)
  -- where (x,y) = enumPairUnsignedInv n

enumSigned :: Int -> Int
enumSigned n
    | n > 0     =  2*(n-1) + 1
    | otherwise = -2*n

-- -- sum (map enumSignedInv [0..1000000])
-- -- (1.85 secs, 544,122,848 bytes)
-- enumSignedInv :: Int -> Int
-- enumSignedInv n
    -- | r == 0    = q
    -- | otherwise = -q
  -- where (q,r) = (n+1) `divMod` 2

-- -- sum (map enumSignedInv [0..1000000])
-- -- (1.85 secs, 544,122,848 bytes)
enumSignedInv :: Int -> Int
enumSignedInv n
    | r == 0    = 1
    | otherwise = -1
  where (q,r) = (n+1) `divMod` 2

f1 y
    | floor x `mod` 2 == 1 =  (floor  (x/2)   )
    | otherwise            =  (floor ((x+1)/2))
  where x = y + 1
-- ghci> map f1 [0..20]
-- [0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10]

f2 y
    | floor x `mod` 2 == 1 = -(floor  (x/2)   )
    | otherwise            =  (floor ((x+1)/2))
  where x = y + 1
-- ghci> map f2 [0..20]
-- [0,1,-1,2,-2,3,-3,4,-4,5,-5,6,-6,7,-7,8,-8,9,-9,10,-10]

f3 y
    | floor x `mod` 2 == 1 = -enumInv (floor  (x/2)   )
    | otherwise            =  enumInv (floor ((x+1)/2))
  where x = y + 1
-- ghci> map f3 [0..20]
-- [0,1,-1,1,-1,2,-2,2,-2,2,-2,3,-3,3,-3,3,-3,3,-3,4,-4]

g = enumInv . f1
-- ghci> map g [0..20]
-- [0,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4]

g2 n = floor ((sqrt (8*n+1) - 1)/2)
-- ghci> map g2 [0..20]
-- [0,1,1,2,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5,5,5]

g3 0 = 0
g3 x'
    | case4 x == 1 = (+1) . peakSize . fromInt . duper $ x
    | case4 x == 2 = (+1) . peakSize . fromInt . duper $ x
    | case4 x == 3 = (+1) . peakSize . fromInt . duper $ x
    | otherwise    = (+1) . peakSize . fromInt . duper $ x
  where x = x' - 1
-- ghci> map g3 [0..100]
-- [0,0,0,0,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6]

t1a1 0 = 0
t1a1 x'
    | case4 x == 1 = y1 - y2
    | case4 x == 2 = y1 - y2
    | case4 x == 3 = y1 - y2
    | otherwise    = y1 - y2
  where x = x' - 1
        y1 = (+1) . peakSize . fromInt . duper $ x
        y2 = (+1) . peakSize . fromInt . duper $ x+2

duper x = floor (x/4)
fromInt = fromInteger . toInteger
peakSize n = floor ((sqrt (8*n+1) - 1)/2)
case4 x = floor (x+1) `mod` 4

n y = (1/8) * (2*y + 1)^2 - 1

intermediateA12 = [0..]
-- intermediateA12 = [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40]

-- Make the following using flooring of inverse quadratics in the same was as was done with peakSize from (1/8) * (2*y + 1)^2 - 1 which has differences between values at integers of [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0] and use them to define intermediateH1 and intermediateH2
intermediateB12 = [  0,  1,  1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  6,  7,  7,  8,  8,  9,  9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20]
intermediateC1  = [  0,  1,  1,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  5,  5,  5,  5,  5,  5,  5,  5]
intermediateC2  = [  0,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4]
intermediateG1  = [  0,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4]
intermediateG2  = [  0,  0,  0,  1,  1,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  4,  4]

intermediateD1 = alternate        <$> [0..]
-- intermediateD12 = [  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1]
intermediateD2 = alternate . (+1) <$> [0..]
-- intermediateD12 = [- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1]
intermediateE1  = alternate        <$> intermediateG1
-- intermediateE1  = [  1,- 1,- 1,- 1,- 1,- 1,- 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1]
intermediateE2  = alternate        <$> intermediateG2
-- intermediateE2  = [  1,  1,  1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,- 1,  1,  1]
intermediateF1  = getZipList $ (*) <$> ZipList intermediateD1 <*> ZipList intermediateE1
-- intermediateF1  = [  1,  1,- 1,  1,- 1,  1,- 1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1]
intermediateF2  = getZipList $ (*) <$> ZipList intermediateD2 <*> ZipList intermediateE2
-- intermediateF2  = [- 1,  1,- 1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,- 1,  1,  1,- 1]

alternate i = if i `mod` 2 == 0 then 1 else -1

-- define intermediateH1 and intermediateH2 using intermediateB12, intermediateC1, intermediateC2, intermediateG1 and intermediateG2
intermediateH1  = [  0,  0,  0,  1,  1,  1,  1,  0,  0,  1,  1,  2,  2,  2,  2,  1,  1,  0,  0,  1,  1,  2,  2,  3,  3,  3,  3,  2,  2,  1,  1,  0,  0,  1,  1,  2,  2,  3,  3,  4,  4]
intermediateH2  = [  0,  1,  1,  0,  0,  1,  1,  2,  2,  1,  1,  0,  0,  1,  1,  2,  2,  3,  3,  2,  2,  1,  1,  0,  0,  1,  1,  2,  2,  3,  3,  4,  4,  3,  3,  2,  2,  1,  1,  0,  0]

target1         = getZipList $ (*) <$> ZipList intermediateH1 <*> ZipList intermediateF1
-- target1         = [  0,  0,  0,  1,- 1,  1,- 1,  0,  0,- 1,  1,- 2,  2,- 2,  2,- 1,  1,  0,  0,  1,- 1,  2,- 2,  3,- 3,  3,- 3,  2,- 2,  1,- 1,  0,  0,- 1,  1,- 2,  2,- 3,  3,- 4,  4]
target2         = getZipList $ (*) <$> ZipList intermediateH2 <*> ZipList intermediateF2
-- target2         = [  0,  1,- 1,  0,  0,- 1,  1,- 2,  2,- 1,  1,  0,  0,  1,- 1,  2,- 2,  3,- 3,  2,- 2,  1,- 1,  0,  0,- 1,  1,- 2,  2,- 3,  3,- 4,  4,- 3,  3,- 2,  2,- 1,  1,  0,  0]


-- attempt1 = ZipList 
attempt2 = xs
  where xs = getZipList $ (*) <$> ZipList target2 <*> ZipList intermediateF2
        