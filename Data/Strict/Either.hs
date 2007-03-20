-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Either
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict @Either@.
--
-- Same as the standard Haskell @Either@, but @Left _|_ = Right _|_ = _|_@
--
-----------------------------------------------------------------------------

module Data.Strict.Either (
    Either(..)
  , either
) where

import Prelude hiding( Either(..), either )

-- | The strict choice type.
data Either a b = Left !a | Right !b deriving(Eq, Ord, Read, Show)

instance Functor (Either a) where
  fmap _ (Left  x) = Left x
  fmap f (Right y) = Right (f y)

-- | Case analysis: if the value is @'Left' a@, apply the first function to @a@;
-- if it is @'Right' b@, apply the second function to @b@.
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left  x) = f x
either _ g (Right y) = g y


