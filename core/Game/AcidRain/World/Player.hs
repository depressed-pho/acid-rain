{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Player
  ( Permission(..)
  , Player(..)
  , PlayerID
  ) where

import Data.UUID (UUID)
import Game.AcidRain.World.Position (WorldPos)


data Permission =
  -- | Each world has exactly one root player. The root player is the
  -- owner of the world, and can use any operations including cheat
  -- commands. When a player enters a single-player world, the player
  -- becomes the root regardless of the player identity.
  Root |
  -- | Administrators can use any operations including cheat commands
  -- and op'ing and deop'ing any players.
  Administrator |
  -- | Members can play the game as normal, but cannot use certain
  -- op-only commands.
  Member |
  -- | Visitors cannot break tiles or pick items. They can still
  -- interact with tiles though.
  Visitor
  deriving (Show, Eq)


data Player = Player
  { plID   ∷ !PlayerID
  , plPerm ∷ !Permission
  , plPos  ∷ !WorldPos
  } deriving (Show)


type PlayerID = UUID
