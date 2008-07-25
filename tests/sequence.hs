
{-


Test cases for Control.Monad.Strict RULES pragma

Compile with -Onot to see if primed function work correctly.
Compile with -O -ddump-simpl-stats to see if rules fired.


-}


import Text.Printf
import System.CPUTime
import Control.Monad.Strict
import Data.IORef
import Control.Monad

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    v `seq` return ()
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main = do
    v <- newIORef []

    -- unmapM
    -- unmapM'
    let g = readIORef v >>= \a -> case a of
                    (x:xs) -> writeIORef v xs >> return (Just x)
                    _ -> return Nothing

    writeIORef v [1..100]
    a <- unmapM g
    writeIORef v [1..100]
    b <- unmapM' g
    print (a==b)

    -- sequence'
    a <- sequence' (map return [1..100])
    b <- sequence (map return [1..100])
    print (a==b)

    let h x = modifyIORef v (x:)
    writeIORef v []
    sequence' (map h [1..100])
    a <- readIORef v
    writeIORef v []
    sequence (map h [1..100])
    b <- readIORef v
    print (a==b)


    -- mapM'
    writeIORef v []
    mapM' h [1..100]
    a <- readIORef v
    writeIORef v []
    mapM h [1..100]
    b <- readIORef v
    print (a==b)

    -- forM'
    writeIORef v []
    forM' [1..100] h
    a <- readIORef v
    writeIORef v []
    forM [1..100] h
    b <- readIORef v
    print (a==b)

    -- unfoldM
    -- unfoldM'
    let g z = readIORef v >>= \a -> case a of
                    (x:xs) -> writeIORef v xs >> return (Just (x,z))
                    _ -> return Nothing
    writeIORef v [1..100]
    a <- unfoldM g 0
    writeIORef v [1..100]
    b <- unfoldM' g 0
    print (a==b)

    -- zipWithM'
    a <- zipWithM' (\a b -> return (a+b)) [1..10] [1..10]
    b <- zipWithM (\a b -> return (a+b)) [1..10] [1..10]
    print (a==b)

    -- filterM'
    a <- filterM' (\a -> return (a>5)) [1..10]
    b <- filterM (\a -> return (a>5)) [1..10]
    print (a==b)

    -- replicateM'
    let g = readIORef v >>= \a -> case a of
                    (x:xs) -> writeIORef v xs >> return (Just x)
                    _ -> return Nothing
    writeIORef v [1..100]
    a <- replicateM' 20 g
    writeIORef v [1..100]
    b <- replicateM 20 g
    print (a==b)

    -- mapAndUnzipM'
    a <- mapAndUnzipM' (\(a,b) -> return (a,b)) [ (a,b) | a <- [1..10], b <- [1..10]]
    b <- mapAndUnzipM (\(a,b) -> return (a,b)) [ (a,b) | a <- [1..10], b <- [1..10]]
    print (a==b)


