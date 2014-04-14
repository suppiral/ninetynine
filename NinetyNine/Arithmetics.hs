module Arithmetics
( isPrime
, primes
, gcd
, coprime
, totient
, primeFactors
) where

import NinetyNine10 (encode)
import Data.List (foldl')

mapWhile :: (a -> Bool) -> [a] -> Bool
mapWhile f [] = False
mapWhile f (x:xs)
    | f x == True = True
    | otherwise = mapWhile f xs

-- 31

isPrime :: Integral a => a -> Bool
isPrime x
    | x < 2 = False
    | otherwise = not $ mapWhile (isModZero x) $ takeWhile (<=(ceiling . sqrt . fromIntegral) x) [2..]
         where isModZero n y = n `mod` y == 0 && n /= 2

primes :: Integral a => [a]
primes = [n | n <- [2..], isPrime n]

-- 32

gcd' :: (Integral a) => a -> a -> a
gcd' a b
    | a < b = gcd' b a
    | b == 0 = a
    | otherwise = gcd' b (a `mod` b)

-- 33

coprime :: (Integral a) => a -> a -> Bool
coprime a b = gcd' a b == 1

-- 34

totient :: Integral a => a -> Int
totient n = length $ filter (coprime n) [1..n]

-- 35

primeFactors :: Integral a => a -> [a]
primeFactors n
    | n < 2 = []
    | otherwise = case filter (not . coprime n) (potenNums n) of
        [] -> []
        (m:rest) ->  m : primeFactors (n `div` m)
        where potenNums x = takeWhile (<= x) primes

primeFactorsMult :: Integral a => a -> [(a,Int)]
primeFactorsMult = map flipTuple . encode . primeFactors
    where flipTuple (x,y) = (y,x)

multPrimeFactors :: Integral a => [(a, Int)] -> a
multPrimeFactors = foldl' (*) 1 . map f
    where f (x,y) = x ^ y
