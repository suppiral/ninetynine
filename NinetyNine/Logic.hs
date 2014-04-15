module Logic
( not', and',or',nor',nand',xor',impl',equ'
, table
) where

import Control.Monad (replicateM)
import Control.Applicative
import Data.List (foldl', foldl1)

not' :: Bool -> Bool
not' True = False
not' False = True

and',or',nor',nand',xor',impl',equ' :: Bool -> Bool -> Bool

and' True True = True
and' _ _ = False

or' False False = False
or' _ _ = True

nor' a = not . or' a

nand' a = not . and' a

xor' True True = False
xor' False False = False
xor' _ _ = True

impl' True False = False
impl' _ _ = True

equ' a = not . xor' a

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn $ g f <$> [False, True] <*> [False, True]
    where g f a b = show a ++ " " ++ show b ++ " " ++ show (f a b)

-- 47
infixl 6 `and'`
infixl 4 `or'`


-- 48

and'',or'',nor'',nand'',xor'',impl'',equ'' :: [Bool] -> Bool

and'' = foldl' and' True

or'' = foldl' or' False

nor'' = not . or''

nand'' = not . and''

xor'' = foldl' xor' False

impl'' zs@(x:xs) = and'' $ zipWith impl' zs xs

equ'' zs@(x:xs) = and'' $ zipWith equ' zs xs

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn $ map (\a -> listToStr a ++ " " ++ (show . f) a) $ replicateM n [False, True]
    where listToStr = unwords . map show
