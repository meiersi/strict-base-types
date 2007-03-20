-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict versions of some standard Haskell types.
--
-----------------------------------------------------------------------------

module Data.Strict (
    module Data.Strict.Tuple
  , module Data.Strict.Maybe
  , module Data.Strict.Either
) where

import Data.Strict.Tuple
import Data.Strict.Tuple  as S
import Data.Strict.Maybe
import Data.Strict.Maybe  as S
import Data.Strict.Either
import Data.Strict.Either as S

infixr 0 `sseq`

class Strict a where
  sseq :: a -> b -> b

  {-# INLINE sseq #-}
  sseq = seq

instance Strict ()
instance Strict Bool
instance Strict Char
instance Strict Int
instance Strict Float
instance Strict Double

instance (Strict a, Strict b) => Strict (S.Pair a b) where
  x S.:!: y `sseq` z = x `sseq` y `sseq` z

instance Strict a => Strict (S.Maybe a) where
  S.Nothing `sseq` y = y
  S.Just x  `sseq` y = x `sseq` y

instance (Strict a, Strict b) => Strict (S.Either a b) where
  S.Left  x `sseq` y = x `sseq` y
  S.Right x `sseq` y = x `sseq` y

