{-# LANGUAGE GADTs #-}

-- https://byorgey.wordpress.com/2017/01/27/advent-of-code-16-solution-an-algebra-of-bitstrings/

import Control.Arrow   ((***))
import Data.Bits       (xor)
import Data.List       (unfoldr)
import Data.List.Split (chunksOf)
import Data.Maybe      (fromJust)

type BitString = [Bool]

readbits :: String -> BitString
readbits = map (=='1')

showbits :: BitString -> String
showbits = map (\x -> if x then '1' else '0')

withbits :: (BitString -> BitString) -> String -> String
withbits f = showbits . f . readbits

invert :: BitString -> BitString
invert = map not

dragon :: BitString -> BitString
dragon a = a ++ [False] ++ invert (reverse a)

fill :: Int -> BitString -> BitString
fill n = take n . head . dropWhile ((< n) . length) . iterate dragon

checksum :: BitString -> BitString
checksum = head . dropWhile (even . length) . iterate go
    where go = map xnor . chunksOf 2
          xnor [x,y] = x == y

data BitExpr where
    Emp :: BitExpr
    Bit :: Bool -> BitExpr
    App :: !Bool -> !Integer -> BitExpr -> BitExpr -> BitExpr
    Inv :: !Bool -> !Integer -> BitExpr -> BitExpr
    Rev :: !Bool -> !Integer -> BitExpr -> BitExpr
    Drg :: !Bool -> !Integer -> BitExpr -> BitExpr
    deriving Show

toBits :: BitExpr -> BitString
toBits Emp = []
toBits (Bit b) = [b]
toBits (App _ _ s1 s2) = toBits s1 ++ toBits s2
toBits (Inv _ _ s) = invert  (toBits s)
toBits (Rev _ _ s) = reverse (toBits s)
toBits (Drg _ _ s) = dragon  (toBits s)

bsLen :: BitExpr -> Integer
bsLen Emp           = 0
bsLen (Bit _)       = 1
bsLen (App _ l _ _) = l
bsLen (Inv _ l _)   = l
bsLen (Rev _ l _)   = l
bsLen (Drg _ l _)   = l

bsXor :: BitExpr -> Bool
bsXor Emp           = False
bsXor (Bit b)       = b
bsXor (App b _ _ _) = b
bsXor (Inv b _ _)   = b
bsXor (Rev b _ _)   = b
bsXor (Drg b _ _)   = b

bit :: Bool -> BitExpr
bit = Bit

app :: BitExpr -> BitExpr -> BitExpr
app s1 Emp = s1
app s1 s2 = App (bsXor s1 `xor` bsXor s2) (bsLen s1 + bsLen s2) s1 s2

bits :: String -> BitExpr
bits = foldr (app . bit . (=='1')) Emp

inv :: BitExpr -> BitExpr
inv s = Inv (if even (bsLen s) then bsXor s else not (bsXor s))
            (bsLen s)
            s

rev :: BitExpr -> BitExpr
rev s = Rev (bsXor s) (bsLen s) s

drg :: BitExpr -> BitExpr
drg s = Drg (bsXor s `xor` bsXor (inv s)) (2*(bsLen s) + 1) s

splitBits :: Integer -> BitExpr -> (BitExpr, BitExpr)
splitBits 0 s = (Emp, s)
splitBits n s | n >= bsLen s = (s,Emp)
splitBits n (App _ _ s1 s2)
    | n < bsLen s1 =
        let (x,y) = splitBits n s1 in (x, y `app` s2)
    | otherwise =
        let (x,y) = splitBits (n - bsLen s1) s2 in (s1 `app` x, y)
splitBits n (Inv _ _ s) = (inv *** inv) $ splitBits n s
splitBits n (Rev _ _ s) = splitBits n (pushRev s)
splitBits n (Drg _ _ s) = splitBits n (expandDragon s)

pushRev :: BitExpr -> BitExpr
pushRev (App _ _ s1 s2) = rev s2 `app` rev s1
pushRev (Inv _ _ s)    = inv $ rev s
pushRev (Rev _ _ s)    = s
pushRev (Drg _ _ s)    = drg $ inv s

expandDragon :: BitExpr -> BitExpr
expandDragon s = s `app` (bit False `app` inv (rev s))

fillE :: Integer -> String -> BitExpr
fillE n = fst . splitBits n . head . dropWhile ((< n) . bsLen) . iterate drg . bits

-- FIXME!!!
checksumE :: BitExpr -> BitString
checksumE s = go (blockSize (bsLen s)) s
    where
        go _ Emp = [] 
        go n s' = let (h,t) = splitBits n s'
                  in not (bsXor h) : go n t
        blockSize n
            | odd n = 1
            | otherwise = 2 * blockSize (n `div` 2)

blockSize :: Integer -> Integer

checksumE1 :: BitExpr -> BitString
checksumE1 s = map (not . bsXor) . unfoldr doSplit $ s
  where
    doSplit Emp = Nothing
    doSplit s   = Just (splitBits blockLen s)
    blockLen = powTwo (bsLen s)
    powTwo n
      | odd n     = 1
      | otherwise = 2 * powTwo (n `div` 2)
