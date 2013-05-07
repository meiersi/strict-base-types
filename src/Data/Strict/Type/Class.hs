{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Type.Class
-- Copyright   :  (c) 2013 Simon Meier
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Simon Meier <iridcode@gmail.com>
-- Stability   :  experimental
-- Portability :  TypeFamilies
--
-- A type-class characterizing types that are strict variants of lazy types.
--
-----------------------------------------------------------------------------

module Data.Strict.Type.Class (
  StrictType(..)
) where

-- | This type-class characterizes types that are strict variants of lazy
-- types.
class StrictType strict where
    type LazyVariant strict

    -- | Convert a strict type to its lazy variant.
    toLazy   :: strict -> LazyVariant strict

    -- | Convert from the lazy variant to the strict type.
    toStrict :: LazyVariant strict -> strict
