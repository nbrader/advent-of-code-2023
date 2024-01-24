#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package QuickCheck-2.14.3

{-# LANGUAGE ApplicativeDo #-}

module Util where

import Control.Applicative
-- import Test.QuickCheck
import Data.Bits
import Data.List

-- A pairing function for signed integers
signedPairing :: (Integer, Integer) -> Integer
signedPairing (i, j) = pairing (inverseEnumSigned' i, inverseEnumSigned' j)

-- Please find corresponding inverses to these functions
-- An unpairing function for signed integers
signedUnpairing :: Integer -> (Integer, Integer)
signedUnpairing z = let (i, j) = unpairing z
                    in (enumSigned' i, enumSigned' j)

enumSigned' :: Integer -> Integer
enumSigned' z = let absZ =  z+1
                    signZ = absZ .&. 1
                    i = (absZ `shiftR` 1) * if signZ == 0 then 1 else -1
                in i

inverseEnumSigned' i = if i <= 0 then -2*i else 2*i-1

-- The original pairing function, updated to work with absolute values and sign bits
pairing :: (Integer, Integer) -> Integer
pairing (i, j) = if i == 0 && j == 0 then 0 else
                  let i0 = i .&. 1
                      j0 = j .&. 1
                      nextPair = pairing (i `shiftR` 1, j `shiftR` 1)
                  in (i0 .|. (j0 `shiftL` 1)) .|. (nextPair `shiftL` 2)

-- The original unpairing function, used as is
unpairing :: Integer -> (Integer, Integer)
unpairing z = unpair z 0 0 0
  where
    unpair 0 _ i j = (i, j)
    unpair n bit i j = let iBit = n .&. 1
                           jBit = (n `shiftR` 1) .&. 1
                           newI = i .|. (iBit `shiftL` bit)
                           newJ = j .|. (jBit `shiftL` bit)
                       in unpair (n `shiftR` 2) (bit + 1) newI newJ

-- A function to calculate the length of the pairing function result
pairLength :: Integer -> Integer -> Integer
pairLength i j = 2 * (integerLog2 $ max i j)

-- Helper function to calculate the log base 2 of an Integer
integerLog2 :: Integer -> Integer
integerLog2 0 = error "log2 of 0"
integerLog2 n = go n 0
  where
    go 1 acc = acc
    go x acc = go (x `shiftR` 1) (acc + 1)


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

m y = (1/8) * (2*y + 1)^2 - 1
n y = y^2 + 2*y
-- n = y^2 + 2*y + 1 - 1
-- n = (y + 1)^2 - 1
-- n + 1 = (y + 1)^2
-- sqrt (n + 1) = y + 1
y n = floor $ sqrt (n + 1) - 1
-- 2*n = y^2 + y

o y = floor $ sqrt (y/2)
-- map n [0..2] :: [Rational]
-- [(-7) % 8,1 % 8,17 % 8]

intermediateA12 = [0..]
-- intermediateA12 = [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40]

-- Make the following using flooring of inverse quadratics in the same was as was done with peakSize from (1/8) * (2*y + 1)^2 - 1 which has differences between values at integers of [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0] and use them to define intermediateH1 and intermediateH2
intermediateB12 =     map (\n -> floor (n/2 + 1/2))              [0..]
-- intermediateB12 = [  0,  1,  1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  6,  7,  7,  8,  8,  9,  9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20]
intermediateC1  = 0 : map (\n -> floor $ sqrt (n/2) + 1)         [0..]
-- intermediateC1  = [  0,  1,  1,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  5,  5,  5,  5,  5,  5,  5,  5]
intermediateC2  = 0 : map (\n -> floor $ sqrt (n/2 + 1/4) + 1/2) [0..]
-- intermediateC2  = [  0,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4]
intermediateG1  = 0 : map (\n -> floor $ sqrt (n/2 + 1))         [0..]
-- intermediateG1  = [  0,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4]
intermediateG2  = 0 : map (\n -> floor $ sqrt (n/2 + 5/4) - 1/2) [0..]
-- intermediateG2  = [  0,  0,  0,  1,  1,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  4,  4]

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

alternate  i = if i `mod` 2 == 0 then 1 else -1
-- alternate' i = sin (pi * (1/2 + i))
-- sum . take 1000000 $ map alternate [0..]
-- 0
-- (0.74 secs, 516,118,688 bytes)
-- ghci> sum . take 1000000 $ map alternate' [0..]
-- 0.0
-- (0.91 secs, 648,120,712 bytes)

-- define intermediateH1 and intermediateH2 using intermediateB12, intermediateC1, intermediateC2, intermediateG1 and intermediateG2
intermediateN1  = map (\n -> let x = floor $ sqrt (n + 1) - 1 in x) [0..]
intermediateK2  = getZipList $ do
    q <- ZipList intermediateM1
    i <- ZipList [0..]
    return $ i - q
-- intermediateK2  = [   0,    1,     2,    0,    1,   2,    3,    4,    0,    1,    2,    3,    4,    5,    6,    0,    1,    2,    3,    4]
intermediateM1  = map (\n -> let x = floor $ sqrt (n + 1) - 1 in (x+1)^2-1) [0..]

intermediateI1 = getZipList $ do
    x <- ZipList intermediateN1
    i <- ZipList intermediateK2
    return $ let q = 2*(x+1)+1 in if i `div` ((q+1) `div` 2) == 0 then i else q-i
-- intermediateI1  = [  0,  1,  1,  0,  1,  2,  2,  1,  0,  1,  2,  3,  3,  2,  1,  0,  1,  2,  3,  4]
intermediateI2 = getZipList $ do
    x <- ZipList intermediateN1
    i <- ZipList intermediateK2
    return $ abs $ (x+1)-i
-- intermediateI2  = [  1,  0,  1,  2,  1,  0,  1,  2,  3,  2,  1,  0,  1,  2,  3,  4,  3,  2,  1,  0]

intermediateH1  = [  0,  0,  0,  1,  1,  1,  1,  0,  0,  1,  1,  2,  2,  2,  2,  1,  1,  0,  0,  1,  1,  2,  2,  3,  3,  3,  3,  2,  2,  1,  1,  0,  0,  1,  1,  2,  2,  3,  3,  4,  4]
intermediateH2  = [  0,  1,  1,  0,  0,  1,  1,  2,  2,  1,  1,  0,  0,  1,  1,  2,  2,  3,  3,  2,  2,  1,  1,  0,  0,  1,  1,  2,  2,  3,  3,  4,  4,  3,  3,  2,  2,  1,  1,  0,  0]

target1         = getZipList $ (*) <$> ZipList intermediateH1 <*> ZipList intermediateF1
-- target1         = [  0,  0,  0,  1,- 1,  1,- 1,  0,  0,- 1,  1,- 2,  2,- 2,  2,- 1,  1,  0,  0,  1,- 1,  2,- 2,  3,- 3,  3,- 3,  2,- 2,  1,- 1,  0,  0,- 1,  1,- 2,  2,- 3,  3,- 4,  4]
target2         = getZipList $ (*) <$> ZipList intermediateH2 <*> ZipList intermediateF2
-- target2         = [  0,  1,- 1,  0,  0,- 1,  1,- 2,  2,- 1,  1,  0,  0,  1,- 1,  2,- 2,  3,- 3,  2,- 2,  1,- 1,  0,  0,- 1,  1,- 2,  2,- 3,  3,- 4,  4,- 3,  3,- 2,  2,- 1,  1,  0,  0]

intermediateI1F :: Int -> Int
intermediateI1F n = if i `div` ((q+1) `div` 2) == 0 then i else q-i
  where x = intermediateN1F n
        i = intermediateK2F n
        q = 2*(x+1)+1

intermediateI2F :: Int -> Int
intermediateI2F n = abs $ (x+1)-i
  where x = intermediateN1F n
        i = intermediateK2F n

intermediateIF :: Int -> (Int, Int)
intermediateIF n = (if i `div` ((q+1) `div` 2) == 0 then i else q-i, abs $ (x+1)-i)
  where x = floor $ sqrt (fromInt n + 1) - 1
        i = n - ((x+1)^2-1)
        q = 2*(x+1)+1

intermediateIF' :: Int -> (Int, Int)
intermediateIF' 0 = (0,0)
intermediateIF' n = (alt11 * alt12 * if i `div` ((q+1) `div` 2) == 0 then i else q-i, alt21 * alt22 * (abs $ (x+1)-i))
  where x = floor $ sqrt ((fromInt (n-1))/2 + 1) - 1
        i = floor ((fromInt (n-1))/2) - ((x+1)^2-1)
        q = 2*(x+1)+1
        alt11 = alternate n
        alt12 = alternate $ floor $ sqrt ((fromInt n-1)/2 + 1)
        
        alt21 = alternate (n+1)
        alt22 = alternate $ floor $ sqrt ((fromInt n-1)/2 + 5/4) - 1/2

intermediateF1F :: Int -> Int
intermediateF1F n = alt1 * alt2
  where alt1 = alternate n
        alt2 = alternate $ floor $ sqrt ((fromInt n-1)/2 + 1)

intermediateD1F n = alternate n
intermediateE1F  = alternate . intermediateG1F
intermediateG1F n = floor $ sqrt ((fromInt n-1)/2 + 1)

intermediateN1F, intermediateK2F, intermediateM1F :: Int -> Int
intermediateN1F n = let y = floor $ sqrt (fromInt n + 1) - 1 in y
intermediateK2F n = n - intermediateM1F n
intermediateM1F n = let y = floor $ sqrt (fromInt n + 1) - 1 in (y+1)^2-1

-- attempt1 = ZipList 
attempt2 = xs
  where xs = getZipList $ (*) <$> ZipList target2 <*> ZipList intermediateF2

success :: Int -> (Int, Int)
success 0 = (0,0)
success n = (alt11 * alt12 * if i `div` ((q+1) `shiftR` 1) == 0 then i else q-i, -alt11 * alt22 * (abs $ (x+1)-i))
  where x = floor $ sqrt ((fromInt n-1)/2 + 1) - 1
        i = ((n-1) `shiftR` 1) - ((x+1)^2-1)
        q = 2*(x+1)+1
        alt11 = alternate n
        alt12 = alternate $ floor $ sqrt ((fromInt n-1)/2 + 1)
        alt22 = alternate $ floor $ sqrt ((fromInt n-1)/2 + 5/4) - 1/2
-- ghci> :set +s
-- ghci> sum $ map ((\(x,y) -> x + y) . success) [0..1000000]
-- 0
-- (11.73 secs, 7,441,239,488 bytes)

success' :: Int -> (Int, Int)
success' 0 = (0,0)
success' n = (alt11 * alt12 * if i `div` ((q+1) `shiftR` 1) == 0 then i else q-i, -alt11 * alt22 * (abs $ (x+1)-i))
  where x = floor $ sqrt ((fromInt n-1)/2 + 1) - 1
        i = ((n-1) `shiftR` 1) - ((x+1)^2-1)
        q = 2*(x+1)+1
        alt11 = alternate n
        alt12 = alternate $ floor $ sqrt ((fromInt n-1)/2 + 1)
        alt22 = alternate $ floor $ sqrt ((fromInt n-1)/2 + 5/4) - 1/2
-- ghci> :set +s
-- ghci> sum $ map ((\(x,y) -> x + y) . success') [0..1000000]
-- ???
-- ???

prop_success_comp :: Int -> Bool
prop_success_comp x = success x == success' x

-- quickCheck prop_success_comp
-- +++ OK, passed 100 tests.
-- (0.00 secs, 750,520 bytes)

test1 = map (\n -> alternate n) [0..50]
test2 = map (\n -> alternate $ floor $ sqrt ((fromInt n-1)/2 + 1)) [0..50]
test3 = map (\n -> alternate (n+1)) [0..50]
test4 = map (\n -> alternate $ floor $ sqrt ((fromInt n-1)/2 + 5/4) - 1/2) [0..50]

tests = [test1, test2, test3, test4]