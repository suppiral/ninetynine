module NinetyNine20
( EncodeItem
, encodeModified
, decodeModified
, encodeDirect
, dupli
, repli
, dropEvery
, split
, slice
, rotate
, removeAt
) where

import NinetyNine10

data EncodeItem a = Multiple Int a | Single a | None deriving (Show)

-- 11
encodeModified :: (Eq a) => [a] -> [EncodeItem a]
encodeModified xs = map (encodeTupleToItem) $ encode xs

encodeTupleToItem :: (Eq a) => (Int, a) -> EncodeItem a
encodeTupleToItem (0, _) = None
encodeTupleToItem (1, x) = Single x
encodeTupleToItem (num, x) = Multiple num x

-- 12
decodeModified :: (Eq a) => [EncodeItem a] -> [a]
decodeModified xs = concatMap decodeItem xs

decodeItem :: (Eq a) => EncodeItem a -> [a]
decodeItem None = []
decodeItem (Single x) = [x]
decodeItem (Multiple num x) = replicate' num x

replicate' :: Int -> a -> [a]
replicate' num x
    | num > 0 = x:(replicate (num-1) x)
    | otherwise = []

-- 13
encodeDirect :: (Eq a) => [a] -> [EncodeItem a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' 1 x xs

encodeDirect' :: (Eq a) => Int -> a -> [a] -> [EncodeItem a]
encodeDirect' num x [] = [encodeTupleToItem (num, x)]
encodeDirect' num x (y:ys)
    | x == y = encodeDirect' (num+1) y ys
    | otherwise = encodeTupleToItem (num, x) : (encodeDirect' 1 y ys)

-- 14
dupli :: [a] -> [a]
dupli xs = concatMap (replicate 2) xs

-- 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n n

dropEvery' :: [a] -> Int -> Int -> [a]
dropEvery' [] _ _ = []
dropEvery' (x:xs) n 1 = dropEvery' xs n n
dropEvery' (x:xs) n count = x:(dropEvery' xs n (count-1))

-- 17
split :: Int -> [a] -> ([a],[a])
split n xs = rev $ split' n [] xs
    where rev (xs, ys) = (reverse xs, ys)

split' :: Int -> [a] -> [a] -> ([a],[a])
split' 0 xs ys = (xs, ys)
split' n xs [] = (xs, [])
split' n xs ys@(z:zs)
    | n > 0 = split' (n-1) (z:xs) zs
    | otherwise = split' (length ys + n) xs ys

-- 18
slice :: Int -> Int -> [a] -> [a]
slice i k list@(x:xs)
    | i > 1 && k > i = slice (i-1) (k-1) xs
    | i == 1 && k > i = fst $ split k list
    | otherwise = []

-- 19
rotate :: Int -> [a] -> [a]
rotate n xs = appendy . switchy $ split n xs
    where switchy (a,b) = (b,a)
          appendy (xs,ys) = xs ++ ys

-- 20
-- We can use Maybe to denote failure of removal due to non-existant member but this is out of the scope of the problem.
removeAt :: Int -> [a] -> (a,[a])
removeAt n xs = remAt $ split (n-1) xs
    where remAt (xs,y:ys) = (y, xs ++ ys)
