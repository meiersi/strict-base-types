{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeFamilies       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Tuple
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
--                (c) 2013 Simon Meier
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Simon Meier <iridcode@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- The strict variant of the standard Haskell pairs and the corresponding
-- variants of the functions from "Data.Tuple".
--
-----------------------------------------------------------------------------


module Data.Strict.Tuple (
    Pair(..)
  , fst
  , snd
  , curry
  , uncurry
  , zip
  , unzip
) where

import           Prelude           hiding (curry, fst, snd, uncurry, unzip, zip)

import           Data.Data
import           Data.Ix
import           Data.Monoid

import           GHC.Generics

import           Data.Strict.Class

-- | The type of strict pairs. Note that
--
-- > (x :!: _|_) = (_|_ :!: y) = _|_
data Pair a b = !a :!: !b
    deriving(Eq, Ord, Show, Read, Bounded, Ix, Data, Typeable, Generic)

instance StrictType (Pair a b) where
    type LazyVariant (Pair a b) = (a, b)

    toStrict (a, b)    = a :!: b
    toLazy   (a :!: b) = (a, b)

instance Functor (Pair e) where
    fmap f = toStrict . fmap f . toLazy

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty          = toStrict mempty
  m1 `mappend` m2 = toStrict (toLazy m1 `mappend` toLazy m2)

{-  To be added once they make it to base

instance Foldable (Pair e) where
  foldMap f (_,x) = f x

instance Traversable (Pair e) where
  traverse f (e,x) = (,) e <$> f x

-}

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

