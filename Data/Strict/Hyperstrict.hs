-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Hyperstrict
-- Copyright   :  (c) Roman Leshchinskiy 2006
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  rl@cse.unsw.edu.au
-- Stability   :  experimental
-- Portability :  portable
--
-- Hyperstrict types.
--
-----------------------------------------------------------------------------

module Data.Strict.Hyperstrict (
  Hyperstrict
) where

import qualified Data.Strict.Tuple  as S
import qualified Data.Strict.Maybe  as S
import qualified Data.Strict.Either as S

import Data.Word
import Data.Int

-- | Instances of 'Hyperstrict' should be hyperstrict types, i.e., those types
-- for which the weak head-normal form and the normal form are the same. This
-- class has no methods; it is only used to increase type safety.
class Hyperstrict a

instance Hyperstrict ()
instance Hyperstrict Bool
instance Hyperstrict Char
instance Hyperstrict Float
instance Hyperstrict Double

instance Hyperstrict Word
instance Hyperstrict Word8
instance Hyperstrict Word16
instance Hyperstrict Word32
instance Hyperstrict Word64

instance Hyperstrict Int
instance Hyperstrict Int8
instance Hyperstrict Int16
instance Hyperstrict Int32
instance Hyperstrict Int64

instance (Hyperstrict a, Hyperstrict b) => Hyperstrict (S.Pair a b)
instance Hyperstrict a => Hyperstrict (S.Maybe a)
instance (Hyperstrict a, Hyperstrict b) => Hyperstrict (S.Either a b)

