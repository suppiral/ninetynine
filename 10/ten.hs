module NinetyNine
( myLast
, myButLast
, elementAt
, myLength
, myReverse
, idList
, isPalindrome
, flatten
, compress
, pack
, encode
) where

-- 1
myLast :: [a] -> a
myLast [] = error "empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast (y:[]) = error "not enough elements"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

-- 3
elementAt :: Int -> [a] -> a
elementAt _ [] = error "too few elements"
elementAt 1 [x] = x
elementAt n (x:xs) 
        | n < 1 = error $ "value smaller than 1: " ++ (show n)
	| n == 1 = x
        | otherwise = elementAt (n-1) xs

-- 4
myLength :: [a] -> Int
myLength = foldl (\acc x -> acc + 1) 0 

-- 5
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) [] 

idList :: [a] -> [a]
idList = foldr (\acc x -> acc:x) []

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = myReverse xs == xs

-- 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

flatten2 :: NestedList a -> [a]
flatten2 (Elem x) = [x]
flatten2 (List xs) = foldl (++) [] $ map flatten2 xs

-- 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = myReverse $ foldl (\acc x -> [x | head acc /= x] ++ acc) [x] (tail xs)

-- 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack xs = takeWhile (==x) xs : pack (dropWhile (==x) xs)
    where x = head xs

pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 [x] = [[x]]
pack2 xs = firstsame : pack rest
    where (firstsame, rest) = span (==head xs) xs

-- 10
encode :: (Eq a) => [a] -> [(Int,a)]
encode xs = map (\x -> (myLength x, head x)) $ pack2 xs
