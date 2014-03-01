module NinetyNine28
( insertAt
, range
, rnd_select
, diff_select
, rnd_permu
, combinations
) where

import NinetyNine20
import System.Random (randomRIO)
import Control.Monad (replicateM)

-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt o xs n = appendo o $ split (n-1) xs
    where appendo o (ys,zs) = ys ++ [o] ++ zs

-- 22
range :: Int -> Int -> [Int]
range a b = [a..b]

rangeAlt a b
    | a <= b = a : (rangeAlt (a+1) b)
    | a > b = []

-- 23
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    indexes <- replicateM n $ randomRIO (0, length xs-1)
    return $ map (xs !!) indexes

-- 24
diff_select :: Int -> Int -> IO [Int]
diff_select n r = rnd_select [1..r] n

-- 25
rnd_permu :: [a] -> IO [a]
rnd_permu xs = do
    perm <- rnd_perm (length xs)
    return (get_perm perm xs)

get_perm :: [Int] -> [a] -> [a]
get_perm [] _ = []
get_perm (p:perm) xs = z : ( get_perm perm (ys++zs))
    where (ys, z:zs) = split p xs

rnd_perm :: Int -> IO [Int]
rnd_perm 1 = return [0]
rnd_perm n = do
    num <- randomRIO (0, n-1)
    rest <- rnd_perm (n-1)
    return (num : rest)

-- 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n [] = []
combinations n (x:xs)
    | n <= length xs + 1 = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)
    | otherwise = []

-- 27

