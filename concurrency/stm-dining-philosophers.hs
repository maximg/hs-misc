
-- Dining philosophers problem solved using STM
-- 
-- Inspired by https://www.schoolofhaskell.com/school/advanced-haskell/beautiful-concurrency/4-the-santa-claus-problem
--
-- Prerequisites:
--   stack install stm random concurrent-output

import Control.Concurrent.STM
import Control.Concurrent
import System.Random
import System.Console.Concurrent


data Fork = MkFork Int (TVar Bool)

newFork :: Int -> STM Fork
newFork n = do
    tv <- newTVar False
    return (MkFork n tv)

takeFork :: Fork -> STM ()
takeFork (MkFork n tv) = do
    isTaken <- readTVar tv
    check(not isTaken)
    writeTVar tv True

putDownFork :: Fork -> STM ()
putDownFork (MkFork n tv) = do
    writeTVar tv False

philosopher1 :: Int -> Fork -> Fork -> IO ()
philosopher1 n f1 f2 = do
    atomically (do  takeFork f1
                    takeFork f2)
    outputConcurrent $ "philosopher " ++ show n ++ " eating...\n"
    randomDelay
    atomically (do  putDownFork f1
                    putDownFork f2)
    outputConcurrent $ "philosopher " ++ show n ++ " thinking...\n"
    randomDelay

main = withConcurrentOutput $ do
    fork1 <- atomically (newFork 1)
    fork2 <- atomically (newFork 2)
    fork3 <- atomically (newFork 3)
    fork4 <- atomically (newFork 4)
    fork5 <- atomically (newFork 5)
    philosopher 1 fork1 fork2
    philosopher 2 fork2 fork3
    philosopher 3 fork3 fork4
    philosopher 4 fork4 fork5
    philosopher 5 fork5 fork1
  where
    philosopher n f1 f2 = forkIO (forever (do philosopher1 n f1 f2
                                              randomDelay))

forever :: IO () -> IO ()
-- Repeatedly perform the action
forever act = forever' act 10
  where -- cheating here to make it stop eventually
    forever' :: IO () -> Int -> IO ()
    forever' act 0 = return ()
    forever' act n = do
        act
        forever' act (n - 1)

randomDelay :: IO ()
-- Delay for a random time between 1 and 1,000,000 microseconds
randomDelay = do waitTime <- getStdRandom (randomR (1, 1000000))
                 threadDelay waitTime
