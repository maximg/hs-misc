import Data.List
import Data.Bits

main1 :: IO ()
main1 = do
    let inp = "2 6 1" --getContents
    print $ words inp
    let (_:cases) = map (read :: String -> Integer) $ words inp
    putStrLn $ unlines $ map (winner . length . unfoldr makeMove) cases

makeMove :: Integer -> Maybe ((), Integer)
makeMove 1 = Nothing
makeMove n = Just $ if even n then ((), n `div` 2)
                              else ((), n - (powTwo n))
powTwo 1 = 1
powTwo n = 2 * powTwo(n `div` 2)

winner :: Int -> String
winner n = if odd n then "Louise"
                    else "Richard"


produce 0 = 0
produce n = n `xor` produce (n-1)

solve :: Int -> [Int] -> Int
solve _ [] = 0
solve 0 _  = 0
solve n [x] = if n `mod` x == 0 then 1 else 0
solve n (x:xs) = foldr (+) 0 $ map solFun $ takeWhile (>= 0) $ iterate (\y -> y - x) n
    where 
        solFun 0 = 1
        solFun k = solve k xs

solh n x = if n `mod` x == 0 then 1 else 0

solve' n (x:xs) = map solFun $ takeWhile ((<= n) . sum) $ iterate (x:) []
    where solFun ys = solve (n - sum ys) xs

tc :: Int -> IO ()
tc n = do
    putStrLn $ unlines $ map toStr $ take n $ zip [0..] [1..]
    where toStr (a,b) = show a ++ " " ++ show b

main :: IO()
main = tc 10000

gen n = [(x,y,z)| x <- [0..n-1], y <- [x..n-1], z <- [y..n-1], 
    x /= y && y /= z && z /= x && x+y+z == n]


runSum acc (x:xs) = (acc + x) : runSum (acc + x) xs
runSum acc [] = []

subsLen s xs = let n = length $ takeWhile (< s) $ runSum 0 $ reverse $ sort xs
    in if n == length xs then 0
                         else n + 1

expon :: Double -> Double
expon = foldr (+) 1 . take 9 . go 1 1
    where go term n x = let term' = (term * x / n) 
                        in term' : go term' (n+1) x

main' :: IO ()
main' = do
    inp <- getContents
    let n = read $ head $ words inp :: Int
    let cases = take n $ map read $ tail $ words inp :: [Double]
    putStrLn $ unlines $ map (show . expon) cases
