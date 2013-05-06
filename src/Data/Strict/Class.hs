{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Class
-- Copyright   :  (c) 2013 Simon Meier
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Simon Meier <iridcode@gmail.com>
-- Stability   :  experimental
-- Portability :  TypeFamilies
--
-- Type-class characterizing types that are strict variants of lazy types.
--
-----------------------------------------------------------------------------

module Data.Strict.Class (
  Strict(..)
) where

class Strict strict where
    type LazyVariant strict

    toLazy   :: strict -> LazyVariant strict
    toStrict :: LazyVariant strict -> strict
