{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeFamilies       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Maybe
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
--                (c) 2013 Simon Meier
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Simon Meier <iridcode@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The strict variant of the standard Haskell 'L.Maybe' type and the
-- corresponding variants of the functions from "Data.Maybe".
-----------------------------------------------------------------------------

module Data.Strict.Maybe (
     Maybe(Nothing,Just)
   , maybe              -- :: b -> (a -> b) -> Maybe a -> b

   , isJust             -- :: Maybe a -> Bool
   , isNothing          -- :: Maybe a -> Bool
   , fromJust           -- :: Maybe a -> a
   , fromMaybe          -- :: a -> Maybe a -> a
   , listToMaybe        -- :: [a] -> Maybe a
   , maybeToList        -- :: Maybe a -> [a]
   , catMaybes          -- :: [Maybe a] -> [a]
   , mapMaybe           -- :: (a -> Maybe b) -> [a] -> [b]
) where

import           Prelude             hiding (Maybe (..), maybe)

import           Control.Applicative
import           Data.Data
import           Data.Foldable       as Foldable
import qualified Data.Maybe          as L
import           Data.Monoid
import           Data.Strict.Class
import           Data.Traversable

import           GHC.Generics

-- | The type of strict optional values.
--
-- In contrast to the standard lazy 'L.Maybe' type, this type is not an
-- applicative functor, and therefore also not a monad. The problem is the
-- /homomorphism/ law, which states that
--
--      @'pure' f '<*>' 'pure' x = 'pure' (f x)@
--
-- must hold for all @f@. This law does not hold for the expected applicative
-- functor instance of 'Maybe', as this instance does not satisfy @pure f
-- \<*\> pure _|_ = pure (f _|_)@ for @f = const@.
data Maybe a = Nothing | Just !a
    deriving(Eq, Ord, Show, Read, Data, Typeable, Generic)

-- instances
------------

instance StrictType (Maybe a) where
  type LazyVariant (Maybe a) = L.Maybe a

  toStrict L.Nothing  = Nothing
  toStrict (L.Just x) = Just x

  toLazy Nothing  = L.Nothing
  toLazy (Just x) = L.Just x

instance Monoid a => Monoid (Maybe a) where
  mempty          = toStrict mempty
  m1 `mappend` m2 = toStrict (toLazy m1 `mappend` toLazy m2)

instance Functor Maybe where
  fmap f = toStrict . fmap f . toLazy

instance Foldable Maybe where
  foldr f y  = Foldable.foldr f y . toLazy
  foldl f y  = Foldable.foldl f y . toLazy

instance Traversable Maybe where
  traverse _ Nothing  = pure Nothing
  traverse f (Just x) = Just <$> f x

-- | Analogous to 'L.isJust' in "Data.Maybe".
isJust :: Maybe a -> Bool
isJust = L.isJust . toLazy

-- | Analogous to 'L.isNothing' in "Data.Maybe".
isNothing :: Maybe a -> Bool
isNothing = L.isNothing . toLazy

-- | Analogous to 'L.fromJust' in "Data.Maybe".
fromJust :: Maybe a -> a
fromJust Nothing  = error "Data.Strict.Maybe.fromJust: Nothing"
fromJust (Just x) = x

-- | Analogous to 'L.fromMaybe' in "Data.Maybe".
fromMaybe :: a -> Maybe a -> a
fromMaybe x = L.fromMaybe x . toLazy

-- | Analogous to 'L.maybe' in "Data.Maybe".
maybe :: b -> (a -> b) -> Maybe a -> b
maybe x f = L.maybe x f . toLazy

-- | Analogous to 'L.listToMaybe' in "Data.Maybe".
listToMaybe :: [a] -> Maybe a
listToMaybe = toStrict . L.listToMaybe

-- | Analogous to 'L.maybeToList' in "Data.Maybe".
maybeToList :: Maybe a -> [a]
maybeToList = L.maybeToList . toLazy

-- | Analogous to 'L.catMaybes' in "Data.Maybe".
catMaybes :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

-- | Analogous to 'L.mapMaybe' in "Data.Maybe".
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) = case f x of
    Nothing -> rs
    Just r  -> r:rs
  where
    rs = mapMaybe f xs
