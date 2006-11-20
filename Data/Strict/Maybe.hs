module Data.Strict.Maybe (
    Maybe(..)
  , isJust
  , isNothing
  , fromJust
  , fromMaybe
  , maybe
) where

import Prelude hiding( Maybe(..), maybe )

data Maybe a = Nothing | Just !a deriving(Eq, Ord, Show, Read)

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

fromJust :: Maybe a -> a
fromJust Nothing  = error "Data.Strict.Maybe.fromJust: Nothing"
fromJust (Just x) = x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just y) = y

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing  = x
maybe _ f (Just y) = f y

