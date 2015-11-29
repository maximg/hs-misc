
-- https://wiki.haskell.org/99_questions/1_to_10

-- 1. find last element of a list
myLast = head . reverse

-- 2. find last but one element of a list
myButLast = head . tail . reverse

-- 3. find nth element of a list, starting with 1
--elementAt    :: [a] -> Int -> a
elementAt x k | k < 1 = error "Index out of bounds"
              | otherwise = (head . drop (k - 1)) x 
