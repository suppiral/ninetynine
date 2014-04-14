module Arithmetics
( isPrime
, primes
, gcd
--, coprime
) where

mapWhile :: (a -> Bool) -> [a] -> Bool
mapWhile f [] = False
mapWhile f (x:xs)
    | f x == True = True
    | otherwise = mapWhile f xs

isPrime :: Int -> Bool
isPrime x
    | x < 2 = False
    | otherwise = not $ mapWhile (isModZero x) $ takeWhile (<=(ceiling . sqrt . fromIntegral) x) [2..]
         where isModZero n y = n `mod` y == 0 && n /= 2

primes :: [Int]
primes = [n | n <- [2..], isPrime n]

gcd' :: (Integral a) => a -> a -> a
gcd' a b
    | a < b = gcd' b a
    | b == 0 = a
    | otherwise = gcd' b (a `mod` b)

coprime :: (Integral a) => a -> a -> Bool
coprime a b = gcd' a b == 1

