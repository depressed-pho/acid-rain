{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Player
  ( Permission(..)
  , Player(..), plID, plPerm, plPos, plIsOnline
  , PlayerID

    -- * Events
  , PlayerMoved(..)
  ) where

import Data.UUID (UUID)
import Game.AcidRain.World.Event (Event(..))
import Game.AcidRain.World.Position (WorldPos)
import Lens.Micro.TH (makeLenses)


data Permission
  = -- | Administrators can use any operations including cheat
    -- commands and op'ing and deop'ing any players.
    Administrator
    -- | Members can play the game as normal, but cannot use certain
    -- op-only commands.
  | Member
    -- | Visitors cannot break tiles or pick items. They can still
    -- interact with tiles though.
  | Visitor
  deriving (Show, Eq)

-- | Player ID is a UUID or any version to uniquely identify a player
-- in a world.
--
-- To join a public world on a remote server for the first time, the
-- client generates its own UUID and an asymmetric key pair only for
-- that server, and saves them locally as a self-signed X.509
-- certificate whose CN is the player ID. The client then establishes
-- an encrypted connection with the server using TLS (or DTLS)
-- protocol. The server saves the client certificate to keep their
-- player record.
--
-- If the server finds that a player with the same ID has joined in
-- the past but the key doesn't match, the server denies the join
-- request and ask the player if they want to join the server using a
-- different identity. After a successful authentication, the player
-- can renew their key simply by sending a newly generated public key
-- to the server (on an encrypted channel of course).
--
-- To prevent MITM attacks, the client also saves the server
-- certificate (which is also self-signed) locally, and if the server
-- key doesn't match while attempting to join the same server
-- subsequently, the client asks the player if they really want to
-- proceed. This is similar to SSH, not like the centralised PKI
-- model.
--
-- To join a private remote world, the client is required to have its
-- certificate registered to the world prior to joining the
-- world. After that there is no difference in the authentication
-- scheme.
--
-- When a world is loaded in single player mode, a special player with
-- the
-- <https://en.wikipedia.org/wiki/Universally_unique_identifier#Nil_UUID Nil UUID>
-- is created if it doesn't exist yet. In this case the
-- player joins the world using the Nil UUID without any public key
-- authentication. This means if a player shares their world file with
-- someone else, other players can freely join the world as the same
-- identity. Nil UUID is not usable in multi player mode. Even the
-- local administrator can't use it.
type PlayerID = UUID

data Player
  = Player
    { _plID       ∷ !PlayerID
    , _plPerm     ∷ !Permission
    , _plPos      ∷ !WorldPos
    , _plIsOnline ∷ !Bool
    } deriving (Show)

makeLenses ''Player

-- | World event to be fired when a player moves. The sole purpose of
-- this event is to let its controlling client update the player
-- offset in 'WorldView', so it is only sent to the controlling client
-- and is not broadcasted. It doesn't even intended to redraw the
-- 'WorldView'.
data PlayerMoved
  = PlayerMoved
    !PlayerID
    !WorldPos -- ^ source
    !WorldPos -- ^ destination
  deriving (Show, Event)
