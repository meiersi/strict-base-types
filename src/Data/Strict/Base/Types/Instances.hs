{-# LANGUAGE TemplateHaskell #-}   -- replace with curly braces,
{-# LANGUAGE OverloadedStrings #-} -- the Haddock prologues are a P.I.T.A!

import           Control.Lens ( (.=), Strict(strict), from, Iso', makeLenses)
import           Control.Monad.State.Strict (State)
import qualified Data.Map                   as M
import qualified Data.Maybe.Strict          as S
import qualified Data.Text                  as T

-- | An example of a state record as it could be used in a (very minimal)
-- role-playing game.
data GameState = GameState
    { _gsCooldown :: !(S.Maybe Int)
    , _gsHealth   :: !Int
    }  -- replace with curly braces, *grmbl*

makeLenses ''GameState

lazy :: Strict lazy strict => Iso' strict lazy
lazy = from strict

type Game = State GameState

cast :: T.Text -> Game ()
cast spell =
    gsCooldown.lazy .= M.lookup spell spellDuration
    -- ... implement remainder of spell-casting ...
  where
    spellDuration = M.fromList [("fireball", 5)]
