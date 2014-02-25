module NinetyNine28
( insertAt
, range
) where

import NinetyNine20

insertAt :: a -> [a] -> Int -> [a]
insertAt o xs n = appendo o $ split (n-1) xs
    where appendo o (ys,zs) = ys ++ [o] ++ zs

range :: Int -> Int -> [Int]
range a b = [a..b]

rangeAlt a b
	| a <= b = a : (rangeAlt (a+1) b)
    | a > b = []
