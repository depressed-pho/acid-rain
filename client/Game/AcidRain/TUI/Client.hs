{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.TUI.Client
  ( Client
  , newClient
  , drawClient
  , isClientClosed
  , handleClientEvent
  ) where

import Brick.BChan (BChan, writeBChan)
import Brick.Types (BrickEvent(..), Location(..), Widget, EventM)
import Control.Concurrent (forkIO)
import Control.Eff (Eff, Member, Lift, Lifted, lift, runLift)
import Control.Eff.State.Strict (State, execState, get, put, modify)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_, foldr')
import Data.Function (fix)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import Data.Maybe (fromJust)
import Data.MultiHashMap.Set.Strict (MultiHashMap)
import qualified Data.MultiHashMap.Set.Strict as MHM
import Data.Unique (Unique, newUnique)
import Game.AcidRain.World.Command
  ( Command(..), CommandType(..), CommandID, SomeCommand
  , IClientCtx(..), ClientCtx )
import Game.AcidRain.World.Player (PlayerID, PlayerMoved(..))
import Game.AcidRain.World.Position (wpX, wpY)
import Game.AcidRain.TUI.AppEvent (AppEvent(..), SomeAppEvent)
import Game.AcidRain.TUI.Keystroke (Keystroke, keystroke)
import Game.AcidRain.TUI.Widgets.WorldView
  ( WorldView, wvWorld, wvPlayer, worldView
  , renderWorldView, redrawWorldView, updatePlayerOffset )
import Game.AcidRain.World
  ( World(..), WorldState(..), WorldStateChanged(..), CommandSetUpdated(..) )
import qualified Game.AcidRain.World.Event as WE
import qualified Graphics.Vty as V
import Lens.Micro ((^.), (.~), (&), (%~), traverseOf)
import Lens.Micro.TH (makeLenses)
import Prelude.Unicode ((∘))


-- | The purpose of the data type 'Client' is to render and handle
-- events for a 'World'. It translates Vty input events into game
-- commands and sends them to the 'World'. It also handles world
-- events and updates Brick widgets accordingly.
data Client
  = Client
    { _cliWorldView    ∷ !(WorldView Unique)
      -- | The latest 'WorldState' we have observed.
    , _cliWorldState   ∷ !(WorldState ())
    , _cliAllCommands  ∷ !(HashSet SomeCommand)
    , _cliKeyMap       ∷ !(MultiHashMap Keystroke SomeCommand)
    , _cliEvDispatcher ∷ !(WE.EventDispatcher () (Eff '[State Client, Lift (EventM Unique)]))
    , _cliClosed       ∷ !Bool
    }

makeLenses ''Client

instance IClientCtx Client where
  basicGetClientPlayerID = (^.cliWorldView.wvPlayer)
  basicSendToWorld cli c args
    = do let w = cli^.cliWorldView.wvWorld
             p = cli^.cliWorldView.wvPlayer
         scheduleCommand w c (Just p) args

newtype ClientEvent = ClientEvent WE.SomeEvent
  deriving Show
instance AppEvent ClientEvent

-- | Create a 'Client' interacting with a given 'World'.
newClient ∷ (World w, MonadIO μ)
          ⇒ Bool     -- ^ Whether to use Unicode characters
          → w        -- ^ The world to interact
          → PlayerID -- ^ The player to interact with the world
          → BChan SomeAppEvent -- ^ Brick application event channel
          → μ Client
newClient uni w pid evChan
  = do name ← liftIO newUnique

       -- We need to redirect world events to the application event
       -- channel, which needs a separate thread.
       void $ liftIO $ forkIO $ fix $ \loop →
         do mwEv ← waitForEvent w
            case mwEv of
              Just wEv → writeBChan evChan (upcastAppEvent $ ClientEvent wEv) *> loop
              Nothing  → return ()

       -- We don't need to send an event to the channel here just to
       -- redraw the client, because we'll soon get
       -- 'WorldStateChanged'.

       ws ← getWorldState w
       return Client
         { _cliWorldView    = worldView name uni w pid
         , _cliWorldState   = () <$ ws
         , _cliAllCommands  = mempty
         , _cliKeyMap       = defaultKeyMap mempty
         , _cliEvDispatcher = initialEventDispatcher
         , _cliClosed       = False
         }

-- | 'Client' possibly consists of multiple overlaying widgets, so
-- this function returns a list of them.
drawClient ∷ Client → [Widget Unique]
drawClient cli
  = [ renderWorldView (cli^.cliWorldView) ]

isClientClosed ∷ Client → Bool
isClientClosed = (^.cliClosed)

handleClientEvent ∷ Client → BrickEvent Unique SomeAppEvent → EventM Unique Client
handleClientEvent cli be
  = case be of
      VtyEvent (V.EvResize _ _) →
        traverseOf cliWorldView redrawWorldView cli

      AppEvent (downcastAppEvent → Just (ClientEvent we)) →
        handleWorldEvent cli we

      _ →
        case cli^.cliWorldState of
          Loading         → handleEventWhileLoading cli be
          LoadPending     → fail "FIXME: not implemented"
          LoadFailed _    → fail "FIXME: not implemented"
          Running _       → handleEventWhileRunning cli be
          Closed (Just _) → fail "FIXME: not implemented"
          Closed Nothing  → return $ cli & cliClosed .~ True

handleEventWhileLoading ∷ Client
                        → BrickEvent Unique SomeAppEvent
                        → EventM Unique Client
handleEventWhileLoading cli be
  = case be of
      VtyEvent (V.EvKey V.KEsc []) →
        return $ cli & cliClosed .~ True
      _ → return cli

handleEventWhileRunning ∷ Client
                        → BrickEvent Unique SomeAppEvent
                        → EventM Unique Client
handleEventWhileRunning cli be
  = case be of
      VtyEvent (V.EvKey V.KEsc []) →
        -- FIXME: Show pause screen
        return $ cli & cliClosed .~ True

      VtyEvent (V.EvKey key mods) →
        do let ks   = keystroke key mods
               cmds = MHM.lookup ks (cli^.cliKeyMap)
           withClientCtx cli $
             traverse_ (flip runOnClient []) cmds

      _ →
        return cli

withClientCtx ∷ MonadIO μ ⇒ Client → Eff '[State ClientCtx, Lift μ] () → μ Client
withClientCtx cli m
  = do ctx ← runLift $ execState (upcastClientCtx cli) $ m
       return $ fromJust $ downcastClientCtx ctx

handleWorldEvent ∷ Client → WE.SomeEvent → EventM Unique Client
handleWorldEvent cli
  = runLift ∘ execState cli ∘ WE.dispatch (cli^.cliEvDispatcher)

initialEventDispatcher ∷ WE.EventDispatcher () (Eff '[State Client, Lift (EventM Unique)])
initialEventDispatcher
  = WE.addHandler handleWSC $
    WE.addHandler handleCSU $
    WE.addHandler handlePM  $
    WE.dispatcher handleAll
  where
    handleCSU ∷ Member (State Client) r ⇒ CommandSetUpdated → Eff r ()
    handleCSU (CommandSetUpdated cs)
      = do modify $ cliAllCommands .~ cs
           modify $ cliKeyMap %~ flip updateKeyMap (defaultKeyMap cs)

    handleWSC ∷ (Member (State Client) r, Lifted (EventM Unique) r)
              ⇒ WorldStateChanged
              → Eff r ()
    handleWSC (WorldStateChanged ws)
      = do modify $ cliWorldState .~ (() <$ ws)
           redraw

    handlePM ∷ (Member (State Client) r, Lifted (EventM Unique) r)
             ⇒ PlayerMoved
             → Eff r ()
    handlePM (PlayerMoved _ src dest)
      = do let δx = fromIntegral $ dest^.wpX - src^.wpX
               δy = fromIntegral $ dest^.wpY - src^.wpY
               δ  = Location (δx, δy)
           cli0 ← get
           cli1 ← lift $ traverseOf cliWorldView (updatePlayerOffset δ) cli0
           put cli1

    -- In theory we could be listening on all the events that can
    -- possibly outdate the world view and be doing nothing in the
    -- fallback handler, but that is very error-prone. Instead we do
    -- the opposite. We listen on individual events that don't need to
    -- redraw the view.
    handleAll ∷ (WE.Event e, Member (State Client) r, Lifted (EventM Unique) r)
              ⇒ e
              → Eff r ()
    handleAll _ = redraw

    redraw ∷ (Member (State Client) r, Lifted (EventM Unique) r) ⇒ Eff r ()
    redraw = do cli0 ← get
                cli1 ← lift $ traverseOf cliWorldView redrawWorldView cli0
                put cli1

defaultKeyMap ∷ HashSet SomeCommand → MultiHashMap Keystroke SomeCommand
defaultKeyMap = foldr' f MHM.empty
  where
    f cmd keyMap
      = case commandType cmd of
          Interactive (Just k) → MHM.insert k cmd keyMap
          _                    → keyMap

-- | Update a key map. Old key mappings will be retained iff their
-- commands also exist in the new one.
updateKeyMap ∷ MultiHashMap Keystroke SomeCommand
             → MultiHashMap Keystroke SomeCommand
             → MultiHashMap Keystroke SomeCommand
updateKeyMap old new = MHM.foldrWithKey' f MHM.empty new
  where
    f ∷ Keystroke
      → SomeCommand
      → MultiHashMap Keystroke SomeCommand
      → MultiHashMap Keystroke SomeCommand
    f newK cmd km
      = case HM.lookup (commandID cmd) revOld of
          Just oldK → MHM.insert oldK cmd km
          Nothing   → MHM.insert newK cmd km

    revOld ∷ HashMap CommandID Keystroke
    revOld = MHM.foldrWithKey' g HM.empty old

    g ∷ Keystroke
      → SomeCommand
      → HashMap CommandID Keystroke
      → HashMap CommandID Keystroke
    g k cmd = HM.insert (commandID cmd) k
