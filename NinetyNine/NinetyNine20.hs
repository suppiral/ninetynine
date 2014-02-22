module NinetyNine20
( EncodeItem
, encodeModified
, decodeModified
, encodeDirect
, dupli
, repli
, dropEvery
) where

import NinetyNine10

data EncodeItem a = Multiple Int a | Single a | None deriving (Show)

encodeModified :: (Eq a) => [a] -> [EncodeItem a]
encodeModified xs = map (encodeTupleToItem) $ encode xs

encodeTupleToItem :: (Eq a) => (Int, a) -> EncodeItem a
encodeTupleToItem (0, _) = None
encodeTupleToItem (1, x) = Single x
encodeTupleToItem (num, x) = Multiple num x

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

encodeDirect :: (Eq a) => [a] -> [EncodeItem a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' 1 x xs

encodeDirect' :: (Eq a) => Int -> a -> [a] -> [EncodeItem a]
encodeDirect' num x [] = [encodeTupleToItem (num, x)]
encodeDirect' num x (y:ys)
    | x == y = encodeDirect' (num+1) y ys
    | otherwise = encodeTupleToItem (num, x) : (encodeDirect' 1 y ys)

dupli :: [a] -> [a]
dupli xs = concatMap (replicate 2) xs

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n n

dropEvery' :: [a] -> Int -> Int -> [a]
dropEvery' [] _ _ = []
dropEvery' (x:xs) n 1 = dropEvery' xs n n
dropEvery' (x:xs) n count = x:(dropEvery' xs n (count-1))
