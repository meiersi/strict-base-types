-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Lazy
-- Copyright   :  (c) Roman Leshchinskiy 2006
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  rl@cse.unsw.edu.au
-- Stability   :  experimental
-- Portability :  portable
--
-- Lazy wrappers.
--
-----------------------------------------------------------------------------

module Data.Strict.Lazy (
    Lazy(..)
  , fromLazy
) where

-- | The lazy wrapper type.
data Lazy a = Lazy a deriving(Eq, Ord, Show, Read)

instance Functor Lazy where
  fmap f (Lazy x) = Lazy (f x)

instance Monad Lazy where
  return       = Lazy
  Lazy x >>= f = f x

-- | Extracts the value wrapped by 'Lazy'.
fromLazy :: Lazy a -> a
fromLazy (Lazy x) = x

