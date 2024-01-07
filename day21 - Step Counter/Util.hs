#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package QuickCheck-2.14.3

module Util where

import Test.QuickCheck
import Data.Function (on)

replace [] _ = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) =
  if n < 0
    then (x:xs)
    else x: replace xs (n-1,a)

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x `seq` (x : iterate' f (f x))

enumPairUnsigned :: (Int,Int) -> Int
enumPairUnsigned (x,y) = ((x+y+1)*(x+y) `div` 2) + y

enumPairUnsignedInv :: Int -> (Int,Int)
enumPairUnsignedInv n = (x,y)
  where n' = fromInteger . toInteger $ n
        w = floor ((sqrt (8*n'+1) - 1)/2)
        t = (w^2+w) `div` 2
        y = n-t
        x = w-y

enumPairSigned :: (Int,Int) -> Int
enumPairSigned (x,y) = enumPairUnsigned (enumSigned x, enumSigned y)

enumPairSignedInv :: Int -> (Int,Int)
enumPairSignedInv n = (enumSignedInv x, enumSignedInv y)
  where (x,y) = enumPairUnsignedInv n

enumSigned :: Int -> Int
enumSigned n
    | n > 0     =  2*(n-1) + 1
    | otherwise = -2*n

-- sum (map enumSignedInv [0..1000000])
-- (1.85 secs, 544,122,848 bytes)
enumSignedInv :: Int -> Int
enumSignedInv n
    | r == 0    = q
    | otherwise = -q
  where (q,r) = (n+1) `divMod` 2

-- sum (map enumSignedInv2 [0..1000000])
-- (3.89 secs, 8,563,703,400 bytes)
-- enumSignedInv2 :: Int -> Int
-- enumSignedInv2 n = ((-1)^n*(1 + 2*n - (-1)^n)) `div` 4

-- sum (map enumSignedInv3 [0..1000000])
-- (4.13 secs, 8,635,707,176 bytes)
-- enumSignedInv3 :: Int -> Int
-- enumSignedInv3 n = ((-1)^(n+1)*(1 + 2*n - (-1)^n)) `div` 4

lerp (x1,x2,t) = (1-t)*x1 + t*x2

-- prop_enumSignedInv_comp :: Int -> Bool
-- prop_enumSignedInv_comp x = x < 0 || ((==) `on` ($ x)) enumSignedInv enumSignedInv3

-- quickCheck prop_enumSignedInv_comp
-- +++ OK, passed 100 tests.
-- (0.00 secs, 750,520 bytes)

-- prop_inv_test_1 :: (Int,Int) -> Bool
-- prop_inv_test_1 (x,y) = x < 0 || y < 0 || (x,y) == (enumPairSignedInv . enumPairSigned $ (x,y))

-- prop_inv_test_2 :: Int -> Bool
-- prop_inv_test_2 x = x < 0 || x == (enumPairSigned . enumPairSignedInv $ x)

-- prop_inv_test_3 :: Int -> Bool
-- prop_inv_test_3 x = x == (enumSignedInv . enumSigned $ x)

-- prop_inv_test_4 :: Int -> Bool
-- prop_inv_test_4 x = x < 0 || x == (enumSigned . enumSignedInv $ x)

-- test1 = quickCheck prop_inv_test_1
-- test2 = quickCheck prop_inv_test_2