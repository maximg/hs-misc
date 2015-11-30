
-- https://wiki.haskell.org/99_questions/1_to_10

-- 1. find last element of a list
myLast = head . reverse

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
