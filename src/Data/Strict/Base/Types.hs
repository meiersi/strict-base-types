{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Base.Types
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
--                (c) 2013 Simon Meier
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Simon Meier <iridcode@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict versions of some standard Haskell types.
--
-----------------------------------------------------------------------------

module Data.Strict.Base.Types (
    module Data.Strict.Tuple
  , module Data.Strict.Maybe
  , module Data.Strict.Either

  , module Data.Strict.Class
) where


import           Data.Strict.Either
import           Data.Strict.Maybe
import           Data.Strict.Maybe          as S
import           Data.Strict.Tuple

import           Control.Lens
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           Data.Strict.Class
import qualified Data.Strict.Maybe          as S
import qualified Data.Text                  as T

-- | An example of a state record as it could be used in a (very minimal)
-- role-playing game.
data GameState = GameState
    { _gsCooldown :: !(S.Maybe Int)
    , _gsHealth   :: !Int
    }

makeLenses ''GameState

asStrict :: StrictType t => Iso' (LazyVariant t) t
asStrict = iso toStrict toLazy

asLazy :: StrictType t => Iso' t (LazyVariant t)
asLazy = iso toLazy toStrict

type Game = State GameState

cast :: T.Text -> Game ()
cast spell =
    gsCooldown.asLazy .= M.lookup spell spellDuration
    -- ... implement remainder of spell-casting ...
  where
    spellDuration = M.fromList [("fireball", 5)]

