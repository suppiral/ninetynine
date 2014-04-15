module Logic
( not', and',or',nor',nand',xor',impl',equ'
, table
) where

import Control.Applicative

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
