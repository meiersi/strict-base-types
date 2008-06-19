-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Tuple
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict pairs.
--
-- Same as regular Haskell pairs, but @(x :*: _|_) = (_|_ :*: y) = _|_@
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Strict.Tuple (
    Pair(..)
#ifndef __HADDOCK__
#ifdef __GLASGOW_HASKELL__
  , (:!:)
#endif
#endif
  , fst
  , snd
  , curry
  , uncurry
) where

import Prelude hiding( fst, snd, curry, uncurry )
import Data.Array (Ix)

infixl 2 :!:

-- | The type of strict pairs.
data Pair a b = !a :!: !b deriving(Eq, Ord, Show, Read, Bounded, Ix)

#ifndef __HADDOCK__
#ifdef __GLASGOW_HASKELL__
-- This gives a nicer syntax for the type but only works in GHC for now.
type (:!:) = Pair
#endif
#endif

-- | Extract the first component of a strict pair.
fst :: Pair a b -> a
fst (x :!: _) = x

-- | Extract the second component of a strict pair.
snd :: Pair a b -> b
snd (_ :!: y) = y

-- | Curry a function on strict pairs.
curry :: (Pair a b -> c) -> a -> b -> c
curry f x y = f (x :!: y)

-- | Convert a curried function to a function on strict pairs.
uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f (x :!: y) = f x y

-- | Zip for strict pairs (defined with zipWith).
zip :: [a] -> [b] -> [Pair a b]
zip x y = zipWith (:!:) x y

-- | Unzip for stict pairs into a (lazy) pair of lists.
unzip :: [Pair a b] -> ([a], [b])
unzip x = ( map fst x
          , map snd x
          )

-- | Coerce a lazy pair to a strict pair.
coerceS :: (a, b) -> (Pair a b)
coerceS (a, b) = a :!: b

-- | Coerce a strict pair to a lazy pair.
coerceL :: (Pair a b) -> (a, b)
coerceL (a :!: b) = (a, b)
