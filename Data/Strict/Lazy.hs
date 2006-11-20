module Data.Strict.Lazy (
    Lazy(..)
  , fromLazy
) where

data Lazy a = Lazy a deriving(Eq, Ord, Show, Read)

instance Functor Lazy where
  fmap f (Lazy x) = Lazy (f x)

instance Monad Lazy where
  return       = Lazy
  Lazy x >>= f = f x

fromLazy :: Lazy a -> a
fromLazy (Lazy x) = x

