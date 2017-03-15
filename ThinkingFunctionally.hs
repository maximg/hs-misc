
solveRPN :: String -> Int
solveRPN s = go [] (words s) where
    go stack xs = case xs of
        ("+":xs') -> case stack of
            (x:y:rest) -> go ((x+y):rest) xs'
            _ -> error "Invalid expression"
        (s:xs') -> go (read s:stack) xs'
        [] -> head stack

solveRPN' :: (Num a, Read a) => String -> a  
solveRPN' = head . foldl foldingFunction [] . words  
    where
        foldingFunction (x:y:rest) "+" = (x+y):rest
        foldingFunction stack x = (read x):stack


data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)  
type RoadSystem = [Section]  

heathrowToLondon :: RoadSystem  
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]  

data Label = A | B | C deriving (Show)  
type Path = [(Label, Int)]  

optimalPath :: RoadSystem -> Path
optimalPath xs = let (pathA, pathB) = foldl doSection ([],[]) xs
    in if sum (map snd pathA) < sum (map snd pathB)
            then reverse pathA
            else reverse pathB

doSection :: (Path, Path) -> Section -> (Path, Path)
doSection (pA, pB) (Section a b c)= let
    costA = sum (map snd pA)
    costB = sum (map snd pB)
    pA' = if costA + a < costB + b + c then (A,a):pA
                                       else (C,c):(B,b):pB
    pB' = if costB + b < costA + a + c then (B,b):pB
                                       else (C,c):(A,a):pA
    in (pA',pB')
