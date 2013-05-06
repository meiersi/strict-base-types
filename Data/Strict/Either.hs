{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeFamilies       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Either
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
--                (c) 2013 Simon Meier
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Simon Meier <iridcode@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- The fully strict variant of 'L.Either', which same as the standard Haskell
-- 'L.Either', but @Left _|_ = Right _|_ = _|_@
--
-- See "Data.Either" for the documentation of the exported functions.
--
-----------------------------------------------------------------------------

module Data.Strict.Either (
    Either(..)
  , either
) where

import           Prelude             hiding (Either (..), either)

import           Control.Applicative
import           Data.Data
import qualified Data.Either         as L
import           Data.Foldable
import           Data.Traversable

import           GHC.Generics

import           Data.Strict.Class


-- | The strict choice type.
data Either a b = Left !a | Right !b
    deriving(Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Strict (Either a b) where
    type LazyVariant (Either a b) = L.Either a b
    toStrict (L.Left x)  = Left x
    toStrict (L.Right y) = Right y

    toLazy (Left x)  = L.Left x
    toLazy (Right y) = L.Right y

instance Functor (Either a) where
  fmap f  = toStrict . fmap f . toLazy

instance Foldable (Either a) where
  foldr _ y (Left _)  = y
  foldr f y (Right x) = f x y

  foldl _ y (Left _)  = y
  foldl f y (Right x) = f y x

instance Traversable (Either a) where
  traverse _ (Left x)  = pure (Left x)
  traverse f (Right x) = Right <$> f x

-- | Case analysis: if the value is @'Left' a@, apply the first function to @a@;
-- if it is @'Right' b@, apply the second function to @b@.
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g = L.either f g . toLazy

