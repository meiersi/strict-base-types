{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Maybe
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict @Maybe@.
--
-- Same as the standard Haskell @Maybe@, but @Just _|_ = _|_@
--
-- Note that strict @Maybe@ is not a monad since
-- @ return _|_ >>= f = _|_ @
-- which is not necessarily the same as @f _|_@.
--
-----------------------------------------------------------------------------

module Data.Strict.Maybe (
    Maybe(..)
  , isJust
  , isNothing
  , fromJust
  , fromMaybe
  , maybe
) where

import           Prelude             hiding (Maybe (..), maybe)

import           Control.Applicative
import           Data.Foldable
import qualified Data.Maybe          as L
import           Data.Strict.Class
import           Data.Traversable

-- | The type of strict optional values.
data Maybe a = Nothing | Just !a deriving(Eq, Ord, Show, Read)

instance Strict (Maybe a) where
    type LazyVariant (Maybe a) = L.Maybe a
    toStrict L.Nothing  = Nothing
    toStrict (L.Just x) = Just x

    toLazy Nothing  = L.Nothing
    toLazy (Just x) = L.Just x

instance Functor Maybe where
  fmap f = toStrict . fmap f . toLazy

instance Foldable Maybe where
  foldr _ y Nothing  = y
  foldr f y (Just x) = f x y

  foldl _ y Nothing  = y
  foldl f y (Just x) = f y x

instance Traversable Maybe where
  traverse _ Nothing  = pure Nothing
  traverse f (Just x) = Just <$> f x

-- | Yields 'True' iff the argument is of the form @Just _@.
isJust :: Maybe a -> Bool
isJust = L.isJust . toLazy

-- | Yields 'True' iff the argument is 'Nothing'.
isNothing :: Maybe a -> Bool
isNothing = L.isNothing . toLazy

-- | Extracts the element out of a 'Just' and throws an error if the argument
-- is 'Nothing'.
fromJust :: Maybe a -> a
fromJust Nothing  = error "Data.Strict.Maybe.fromJust: Nothing"
fromJust (Just x) = x

-- | Given a default value and a 'Maybe', yield the default value if the
-- 'Maybe' argument is 'Nothing' and extract the value out of the 'Just'
-- otherwise.
fromMaybe :: a -> Maybe a -> a
fromMaybe x = L.fromMaybe x . toLazy

-- | Given a default value, a function and a 'Maybe' value, yields the default
-- value if the 'Maybe' value is 'Nothing' and applies the function to the
-- value stored in the 'Just' otherwise.
maybe :: b -> (a -> b) -> Maybe a -> b
maybe x f = L.maybe x f . toLazy

