-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Lazy
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  portable
--
-- Lazy identity type.
--
-----------------------------------------------------------------------------

module Data.Lazy ( Lazy(..) )
where

data Lazy a = Lazy { unLazy :: a }
  deriving(Eq, Ord, Read, Show)

instance Functor Lazy where
  fmap f (Lazy x) = Lazy (f x)

