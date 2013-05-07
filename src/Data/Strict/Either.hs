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
-- The strict variant of the standard Haskell 'L.Either' type and the
-- corresponding variants of the functions from "Data.Either".
-----------------------------------------------------------------------------

module Data.Strict.Either (
    Either(..)
  , isRight
  , isLeft
  , either
  , lefts
  , rights
  , partitionEithers
) where

import           Prelude             hiding (Either (..), either)

import           Control.Applicative
import           Data.Data
import qualified Data.Either         as L
import           Data.Foldable
import           Data.Traversable

import           GHC.Generics


-- | The strict choice type.
--
-- Note that this type is not an applicative functor, and therefore also no
-- monad. The reasons are the same as the ones explained in the documentation
-- of the strict 'Data.Strict.Maybe.Maybe' type.
data Either a b = Left !a | Right !b
    deriving(Eq, Ord, Read, Show, Data, Typeable, Generic)

instance StrictType (Either a b) where
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

instance Traversable (Either e) where
  traverse _ (Left x)  = pure (Left x)
  traverse f (Right x) = Right <$> f x


-- | Analogous to 'L.either' in "Data.Either".
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g = L.either f g . toLazy

-- | Analogous to 'L.lefts' in "Data.Either".
lefts   :: [Either a b] -> [a]
lefts x = [a | Left a <- x]

-- | Analogous to 'L.rights' in "Data.Either".
rights   :: [Either a b] -> [b]
rights x = [a | Right a <- x]

-- | Analogous to 'L.partitionEithers' in "Data.Either".
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers =
    Prelude.foldr (either left right) ([],[])
  where
    left  a ~(l, r) = (a:l, r)
    right a ~(l, r) = (l, a:r)

-- | Analogous to 'L.isLeft' in "Data.Either", which will be included in base
-- \> 4.6.
isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

-- | Analogous to 'L.isRight' in "Data.Either", which will be included in base
-- \> 4.6.
isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True

