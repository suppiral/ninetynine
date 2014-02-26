module NinetyNine28
( insertAt
, range
, rnd_select
, diff_select
) where

import NinetyNine20
import System.Random (randomRIO)
import Control.Monad (replicateM)

insertAt :: a -> [a] -> Int -> [a]
insertAt o xs n = appendo o $ split (n-1) xs
    where appendo o (ys,zs) = ys ++ [o] ++ zs

range :: Int -> Int -> [Int]
range a b = [a..b]

rangeAlt a b
    | a <= b = a : (rangeAlt (a+1) b)
    | a > b = []

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    indexes <- replicateM n $ randomRIO (0, length xs-1)
    return $ map (xs !!) indexes

diff_select :: Int -> Int -> IO [Int]
diff_select n r = rnd_select [1..r] n
