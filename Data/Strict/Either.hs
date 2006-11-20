module Data.Strict.Either (
    Either(..)
  , either
) where

import Prelude hiding( Either(..), either )

data Either a b = Left !a | Right !b deriving(Eq, Ord, Read, Show)

instance Functor (Either a) where
  fmap _ (Left  x) = Left x
  fmap f (Right y) = Right (f y)

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left  x) = f x
either f g (Right y) = g y


