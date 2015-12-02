
-- https://wiki.haskell.org/99_questions/1_to_10

{-# LANGUAGE TemplateHaskell #-}
import StaticAssert
import Data.List
import Control.Exception

test' cond = assert cond "PASS"

-- 1. find last element of a list
myLast = head . reverse

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
data CodedSymbol a = Single a | Multiple (Int, a)
    deriving (Show, Eq)

encodeModified xs = map f $ encode xs where
    f (1,x) = Single x
    f (n,x) = Multiple (n,x)

testEncodeModified =
    test' $ encodeModified "aaaabccaadeeee" ==
        [Multiple (4,'a'), Single 'b', Multiple (2,'c'),
         Multiple (2,'a'), Single 'd', Multiple (4,'e')]

-- 12. decode run-length encoded list

decodeModified xs = concatMap decodeHelper xs where
    decodeHelper (Single x)       = [x]
    decodeHelper (Multiple (n,x)) = replicate n x

testDecodeModified = test' $ decodeModified
    [Multiple (4,'a'), Single 'b', Multiple (2,'c'),
    Multiple (2,'a'), Single 'd', Multiple (4,'e')] == "aaaabccaadeeee"
