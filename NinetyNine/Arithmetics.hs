module Arithmetics
( isPrime
, primes
, gcd
, coprime
, totient
, primeFactors
, primeFactorsMult
, multPrimeFactors
, totient2
, primesR
, goldbach
) where

import NinetyNine10 (encode)
import Data.List (foldl')
import Control.Applicative ((<$>), (<*>))

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

-- 36

primeFactorsMult :: Integral a => a -> [(a,Int)]
primeFactorsMult = map flipTuple . encode . primeFactors
    where flipTuple (x,y) = (y,x)

multPrimeFactors :: Integral a => [(a, Int)] -> a
multPrimeFactors = foldl' (*) 1 . map f
    where f (x,y) = x ^ y

-- 37

totient2 :: Integral a => a -> a
totient2 = floor . foldl' (*) 1.0 . map phiPk . primeFactorsMult
    where phiPk (p,k) = (fromIntegral p-1) * (fromIntegral p ** fromIntegral (k-1))


-- 39
primesR :: Integral a => a -> a -> [a]
primesR n m = filter isPrime [n..m]

-- 40

goldbach :: Integral a => a -> (a,a)
goldbach n = snd . head $ filter ((==) n . fst) $ (\x y -> (x+y,(x,y))) <$> primesR 2 n <*> primesR 2 n

-- 41

goldbachList :: Integral a => a -> a -> [(a,a)]
goldbachList n m = (map goldbach . filter even) [n..m]

goldbachList' :: Integral a => a -> a -> a -> [(a,a)]
goldbachList' n m lim = filter ((<) lim . fst) (goldbachList n m)

