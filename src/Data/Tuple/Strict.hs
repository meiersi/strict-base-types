{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- | Copyright :  (c) 2006-2007 Roman Leshchinskiy
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


module Data.Tuple.Strict (
    Pair(..)
  , fst
  , snd
  , curry
  , uncurry
  , zip
  , unzip
) where

import           Data.Strict.Tuple   (Pair ((:!:)), curry, fst, snd, uncurry)
import           Prelude             hiding (curry, fst, snd, uncurry, unzip,
                                      zip)
import qualified Prelude             as L

import           Control.Applicative ((<$>))
import           Control.DeepSeq     (NFData (..))
import           Control.Lens.Iso    (Strict (..), iso)
import           Data.Aeson          (FromJSON (..), ToJSON (..))
import           Data.Binary         (Binary (..))
import           Data.Data           (Data (..), Typeable2 (..))
import           Data.Monoid         (Monoid (..))
#if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics        (Generic (..))
#endif
import           Test.QuickCheck     (Arbitrary (..))

-- Utilities
------------

toStrict :: (a, b) -> Pair a b
toStrict (a, b) = a :!: b

toLazy :: Pair a b -> (a, b)
toLazy (a :!: b) = (a, b)


-- missing instances
--------------------

deriving instance (Data a, Data b) => Data     (Pair a b)
deriving instance Typeable2 Pair

-- fails with compiler panic on GHC 7.4.2
#if __GLASGOW_HASKELL__ >= 706
deriving instance Generic  (Pair a b)
#endif

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty                            = mempty :!: mempty
  (x1 :!: y1) `mappend` (x2 :!: y2) = (x1 `mappend` x2) :!: (y1 `mappend` y2)

-- deepseq
instance (NFData a, NFData b) => NFData (Pair a b) where
  rnf = rnf . toLazy

-- binary
instance (Binary a, Binary b) => Binary (Pair a b) where
  put = put . toLazy
  get = toStrict <$> get

-- aeson
instance (ToJSON a, ToJSON b) => ToJSON (Pair a b) where
  toJSON = toJSON . toLazy

instance (FromJSON a, FromJSON b) => FromJSON (Pair a b) where
  parseJSON val = toStrict <$> parseJSON val

-- quickcheck
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = toStrict <$> arbitrary
  shrink    = map toStrict . shrink . toLazy

-- lens
instance Strict (a, b) (Pair a b) where
  strict = iso toStrict toLazy



{-  To be added once they make it to base

instance Foldable (Pair e) where
  foldMap f (_,x) = f x

instance Traversable (Pair e) where
  traverse f (e,x) = (,) e <$> f x
-}


-- missing functions
--------------------

-- | Zip for strict pairs (defined with zipWith).
zip :: [a] -> [b] -> [Pair a b]
zip x y = zipWith (:!:) x y

-- | Unzip for stict pairs into a (lazy) pair of lists.
unzip :: [Pair a b] -> ([a], [b])
unzip x = ( map fst x
          , map snd x
          )

------------------------------------------------------------------------------
-- Code required to make this module independent of the 'strict' package
------------------------------------------------------------------------------

{-
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
-}

