{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Strict
-- Copyright   :  (c) 2008 Gracjan Polak
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Gracjan Polak <gracjanpolak@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict version of some sequencing functions from @Control.Monad@.
--
-- Primed functions to non-primed are in the same relation as
-- @foldl'@ ot @foldl@. They are faster (about an order of magnitude)
-- use less memory, don't use stack. But have worse laziness properties.
--
-- The @*'rev@ functions return results in reverse order, but are fastes
-- from group. In fact primed functions are implemented using accumulator
-- so results need to be reversed at the end. If you don't need results
-- in order, use @*'rev@ functions to skip @reverse@:
--
-- @*' = *'rev >>= return . reverse@
--
-- WARNING: This module also contains rewrite RULES to make all the
-- standard functions in IO monad faster. Just import me and speed up
-- you programs up to 5 times!
--
-----------------------------------------------------------------------------

module Control.Monad.Strict (
    sequence'
  , sequence'rev
  , mapM'
  , mapM'rev
  , forM'
  , forM'rev
  , zipWithM'
  , zipWithM'rev
  , replicateM'
  , replicateM'rev
  , filterM'
  , filterM'rev
  , mapAndUnzipM'
  , mapAndUnzipM'rev
  , unfoldM
  , unfoldM'
  , unfoldM'rev
  , unmapM
  , unmapM'
  , unmapM'rev
) where

import Control.Monad

{-# RULES
"forM/IO"          forM           = forM'         :: [a] -> (a -> IO b) -> IO [b]
"mapM/IO"          mapM           = mapM'         :: (a -> IO b) -> [a] -> IO [b]
"replicateM/IO"    replicateM     = replicateM'   :: Int -> IO a -> IO [a]
"sequence/IO"      sequence       = sequence'     :: [IO a] -> IO [a]
"unmapM/IO"        unmapM         = unmapM'       :: IO (Maybe a) -> IO [a]
"unfoldM/IO"       unfoldM        = unfoldM'      :: (b -> IO (Maybe (a, b))) -> b -> IO [a]
"zipWithM/IO"      zipWithM       = zipWithM'     :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]
"filterM/IO"       filterM        = filterM'      :: (a -> IO Bool) -> [a] -> IO [a]
"mapAndUnzipM/IO"  mapAndUnzipM   = mapAndUnzipM' :: (a -> IO (b,c)) -> [a] -> IO ([b], [c])
 #-}

-- | Evaluate each action in the sequence from left to right,
-- and collect the results in reverse order.
sequence'rev :: (Monad m) => [m a] -> m [a]
sequence'rev as = helper [] as
    where
        helper vs [] = return vs
        helper vs (m:ms) = m >>= (\v -> helper (v:vs) ms)

-- | Evaluate each action in the sequence from left to right,
-- and collect the results.
sequence' :: (Monad m) => [m a] -> m [a]
sequence' ms = sequence'rev ms >>= return . reverse

-- | @'mapM'' f@ is equivalent to @'sequence'' . 'map' f@.
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
{-# INLINE mapM' #-}
mapM' f as =  sequence' (map f as)

-- | @'mapM'rev' f@ is equivalent to @'sequence'rev' . 'map' f@.
mapM'rev :: Monad m => (a -> m b) -> [a] -> m [b]
{-# INLINE mapM'rev #-}
mapM'rev f as =  sequence'rev (map f as)

-- | 'forM'' is 'mapM'' with its arguments flipped
forM' :: Monad m => [a] -> (a -> m b) -> m [b]
{-# INLINE forM' #-}
forM' = flip mapM'

-- | 'forM'rev' is 'mapM'rev' with its arguments flipped
forM'rev :: Monad m => [a] -> (a -> m b) -> m [b]
{-# INLINE forM'rev #-}
forM'rev = flip mapM'rev

-- | @'replicateM'' n act@ performs the action @n@ times,
-- gathering the results.
replicateM' :: (Monad m) => Int -> m a -> m [a]
replicateM' n x = sequence' (replicate n x)

-- | @'replicateM'rev' n act@ performs the action @n@ times,
-- gathering the results in reverse order
replicateM'rev :: (Monad m) => Int -> m a -> m [a]
replicateM'rev n x = sequence'rev (replicate n x)

-- | This generalizes the list-based 'filter' function.
-- Returns results in reverse order.
filterM'rev :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM'rev p as =  helper [] as
    where
        helper vs [] = return vs
        helper vs (x:xs) = do
           flg <- p x
           if flg
               then helper (x:vs) xs
               else helper vs xs

-- | This generalizes the list-based 'filter' function.
filterM' :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM' p xs =  filterM'rev p xs >>= return . reverse

-- | The 'mapAndUnzipM'' function maps its first argument over a list, returning
-- the result as a pair of lists. This function is mainly used with complicated
-- data structures or a state-transforming monad.
mapAndUnzipM' :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipM' f xs = sequence' (map f xs) >>= return . unzip

-- | The 'mapAndUnzipM'rev' function maps its first argument over a list, returning
-- the result as a pair of lists. This function is mainly used with complicated
-- data structures or a state-transforming monad.
-- Returns results in reverse order.
mapAndUnzipM'rev :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipM'rev f xs = sequence'rev (map f xs) >>= return . unzip

-- | The 'zipWithM'' function generalizes 'zipWith' to arbitrary monads.
zipWithM' :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM' f xs ys  = sequence' (zipWith f xs ys)

-- | The 'zipWithM'rev' function generalizes 'zipWith' to arbitrary monads.
-- Returns results in reverse order.
zipWithM'rev :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM'rev f xs ys = sequence'rev (zipWith f xs ys)

-- | The 'unfoldM' function generalizes 'unfoldr' to arbitrary monads.
-- It is dual to 'foldM', builds a list from seed value in a monad.
unfoldM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldM f base =
    do fb <- f base
       case fb of
           Nothing     -> return []
           Just (a, b) -> do rest <- unfoldM f b
                             return (a : rest)

-- | The 'unfoldM'' function generalizes 'unfoldr' to arbitrary monads.
-- It is dual to 'foldM'', builds a list from seed value in a monad.
-- Returns results in reverse order.
unfoldM'rev :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldM'rev f base = helper [] base
    where
        helper xs b = do
            fb <- f b
            case fb of
                   Nothing     -> return xs
                   Just (a, b1) -> helper (a:xs) b1

-- | The 'unfoldM'' function generalizes 'unfoldr' to arbitrary monads.
-- It is dual to 'foldM'', builds a list from seed value in a monad.
unfoldM' :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldM' f base = unfoldM'rev f base >>= return . reverse

-- |The 'unmapM' function is dual to 'mapM'. It builds a list in a monad.
unmapM :: Monad m => m (Maybe a) -> m [a]
unmapM f =
    do fb <- f
       case fb of
           Nothing -> return []
           Just a -> do rest <- unmapM f
                        return (a : rest)

-- | The 'unmapM'rev' function is dual to 'mapM''. It builds a list in a monad.
-- Returns results in reverse order.
unmapM'rev :: Monad m => m (Maybe a) -> m [a]
unmapM'rev f = helper []
    where
        helper xs = do
            fb <- f
            case fb of
                   Nothing     -> return xs
                   Just a      -> helper (a:xs)

-- | The 'unmapM'' function is dual to 'mapM''. It builds a list in a monad.
unmapM' :: Monad m => m (Maybe a) -> m [a]
unmapM' f = unmapM'rev f >>= return . reverse
