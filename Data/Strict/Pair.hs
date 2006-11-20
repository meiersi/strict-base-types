{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Strict.Pair (
    Pair(..)
#ifdef __GLASGOW_HASKELL__
  , (:*:)
#endif
  , fst
  , snd
  , curry
  , uncurry
) where

import Prelude hiding( fst, snd, curry, uncurry )

infixl 2 :*:

data Pair a b = !a :*: !b deriving(Eq, Ord, Show, Read, Bounded)

#ifdef __GLASGOW_HASKELL__
type (:*:) = Pair
#endif

fst :: a :*: b -> a
fst (x :*: _) = x

snd :: Pair a b -> b
snd (_ :*: y) = y

curry :: (Pair a b -> c) -> a -> b -> c
curry f x y = f (x :*: y)

uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f (x :*: y) = f x y

