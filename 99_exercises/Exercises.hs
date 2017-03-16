
-- https://wiki.haskell.org/99_questions/1_to_10

{-# LANGUAGE TemplateHaskell #-}
import StaticAssert
import Data.List
import Control.Exception
import Data.Ord (comparing)
import Data.Function
import Test.QuickCheck

test' cond = assert cond "PASS"

-- 1. find last element of a list
myLast = head . reverse

-- this test imposes Eq a on the list element type, not clear how to avoid this
--testMyLast' = quickCheck (\xs -> null xs || init xs ++ [myLast xs] == xs)

-- fails with 'myLast must be imported'
-- $(staticAssert (myLast "abcd" == "d") "myLast failed")

testMyLast = test' $ myLast "abcd" == 'd'

-- 2. find last but one element of a list
myButLast = head . tail . reverse

-- 3. find nth element of a list, starting with 1
elementAt x k | k < 1 = error "Index out of bounds"
              | otherwise = (head . drop (k - 1)) x 

-- 4. find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = myLength xs + 1

-- this test requires Show a. Why not in GHCi?
-- testMyLength' = quickCheck (\xs -> myLength xs == length xs)

-- 5. reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 6. check if a list is a palindrome
isPalindrome x = x == reverse x

-- 7. flatten a nested list
data NestedList a = Elem a | List [NestedList a]
flatten              :: NestedList a -> [a]
flatten (Elem x)      = [x]
flatten (List [])     = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- 8. remove duplicates without changing the order
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:(y:xs)) | (x == y)  = x : compress xs 
                    | otherwise = x : compress (y : xs)

-- 9. pack duplicates into sublists

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack x = fst y : pack (snd y) where
    y = span (== head x) x

-- 10. run-length encoding
encode xs = map (\x -> (length x, head x)) $ pack xs

-- 11. modified run-length encoding
data CodeSymbol a = Single a | Multiple (Int, a)
    deriving (Show, Eq)

encodeSymbol (1,x) = Single x
encodeSymbol (n,x) = Multiple (n,x)
encodeSymbol' 1 x = Single x
encodeSymbol' n x = Multiple (n,x)

decodeSymbol (Single x)       = [x]
decodeSymbol (Multiple (n,x)) = replicate n x


encodeModified = map encodeSymbol . encode

testEncodeModified =
    test' $ encodeModified "aaaabccaadeeee" ==
        [Multiple (4,'a'), Single 'b', Multiple (2,'c'),
         Multiple (2,'a'), Single 'd', Multiple (4,'e')]

-- 12. decode run-length encoded list

decodeModified = concatMap decodeSymbol

testDecodeModified = test' $ decodeModified
    [Multiple (4,'a'), Single 'b', Multiple (2,'c'),
    Multiple (2,'a'), Single 'd', Multiple (4,'e')] == "aaaabccaadeeee"

-- 13. direct run-length encoding

-- my solution, simplified after consulting the solutuions
encodeDirect [] = []
encodeDirect (z:zs) = f 1 z zs where
    f n x []     = [encodeSymbol' n x]
    f n x (y:ys) | x == y    = f (n+1) x ys
                 | otherwise = encodeSymbol' n x : encodeDirect (y:ys)

-- from the solutions page, with foldr and an as-pattern
encodeDirect' = map encodeSymbol . foldr f [] where
    f x [] = [(1,x)]
    f x (y@(n,b):ys)
        | x == b    = (n + 1, x):ys
        | otherwise = (1,x):y:ys

testEncodeDirect = 
    test' $ encodeDirect' "aaaabccaadeeee" ==
        [Multiple (4,'a'), Single 'b', Multiple (2,'c'),
         Multiple (2,'a'), Single 'd', Multiple (4,'e')]

-- 14. duplicate elements of a list
dupli xs = concatMap (\x -> [x,x]) xs

testDupli =
    test' $ dupli "abcd" == "aabbccdd"

-- 15. replicate elements of list given number of times
repli xs n = concatMap (replicate n) xs

testRepli =
    test' $ repli "abcd" 3 == "aaabbbcccddd"

-- 16. dron every Nth element of the list
dropEvery [] _ = []
dropEvery xs n | n < 1 = error "Index out of bounds"
               | n == 1 = []
               | otherwise = take (n-1) xs ++ dropEvery (drop n xs) n

testDropEvery =
    test' $ dropEvery "abcdef" 2 == "ace"

-- 17. split a list into 2 parts

split xs n = (reverse a, b) where
    (a,b) = split' ([], xs) n 
    split' (a,[])   _ = (a,[])
    split' (a,b)    0 = (a,b)
    split' (a,b:bs) n = split' (b:a,bs) (n-1)

testSplit =
    test' $ split "abcdefg" 3 == ("abc","defg")

-- 18. slice the list

slice            :: [a] -> Int -> Int -> [a]
slice _ n k      | n < 1 || k < n = error "Index out of bounds"
slice [] _ _     = []
slice (x:xs) 1 1 = [x]
slice (x:xs) 1 k = x : slice xs 1 (k-1)
slice (x:xs) n k = slice xs (n-1) (k-1)

-- with take / drop
slice' xs n k = drop (n-1) $ take k xs

testSlice =
    test' $ slice "abcdefg" 3 5 == "cde"

-- 19. rotate a list N places to the left

rotate xs n = back ++ front where
    (front, back) = split xs pos
    pos = n `mod` length xs

testRotate =
    test' $ rotate "abcdef" (-2) == "efabcd"

-- remove kth element

removeAt k xs
    | k < 1 || k > length xs = error "Index out of bounds"
    | otherwise = (c, h ++ t) where
        (h,(c:t)) = split xs (k-1)

testRemoveAt = 
    test' $ removeAt 3 "abcdef" == ('c',"abdef")

-- 28a. sort list according to length of sublists, placing the shortest first

lsort :: [[a]] -> [[a]]
lsort = sortOn length

testLsort =
    test' $ lsort ["a","cd","b","abcd"] == ["a","b","cd","abcd"]

-- 28b. sort lists according to length of sublists, placing the rarest lengths first
lsort' :: [[a]] -> [[a]]
lsort' xs = let
    withLengths xs = map (\a -> ((length a),a)) xs
    lengthFreq len xs = length $ filter (\(a,_) ->  a == len) $ withLengths xs
    freqs xs = map (\(l,b) -> ((lengthFreq l xs),b)) $ withLengths xs
    in map snd $ sortOn fst $ freqs xs

lfsort = concat . lsort . groupBy ((==) `on` length) . lsort

-- 31. isPrime

isPrime :: Integral a => a -> Bool
isPrime n = (factors n) == [1,n] where
    factors n = [ x | x <- [1..n], n `mod` x == 0]

testIsPrime =
   test' $ isPrime 11

-- https://en.wikibooks.org/wiki/Haskell/Monoids
import Data.Monoid

-- | Monoid under addition.
data Sum = Sum { getSum :: Bool } deriving (Show)

-- | Monoid under multiplication.
data Product = Product { getProduct :: Bool } deriving (Show)

instance Monoid (Product) where
    mempty = Product True
    Product x `mappend` Product y = Product (x && y)

instance Monoid (Sum) where
    mempty = Sum False
    Sum x `mappend` Sum y = Sum (x || y)

Product True <> (Product True <> Product True)
(Product True <> Product True) <> Product True
Product True <> mempty

-- https://en.wikibooks.org/wiki/Haskell/Applicative_functors
data Tree a = Node a [Tree a] deriving (Show)
instance Functor Tree where
    fmap f (Node x xs) = Node (f x) (map (fmap f) xs)

fmap (+1) (Node 5 [Node 6 []])


data MyEither a b = MyLeft a | MyRight b deriving (Show)
instance Functor (MyEither e) where
    fmap _ (MyLeft x)  = MyLeft x
    fmap f (MyRight y) = MyRight (f y)
    
fmap (+1) (MyLeft 1)
fmap (+1) (MyRight 2)


-- Write a definition of (<*>) using (>>=) and fmap. Do not use do-notation.
-- <*> :: Applicative f => f (a -> b) -> f a -> f 
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- fmap :: Functor f => (a -> b) -> f a -> f b

f <*> x = f >>= (`fmap` x)


