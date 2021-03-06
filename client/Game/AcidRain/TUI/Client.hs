{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Brick.Widgets.Core (Named(..))
import Control.Concurrent (forkIO)
import Control.Eff (Eff, Member, Lift, Lifted, lift, runLift)
import Control.Eff.Exception (Exc, runError)
import Control.Eff.State.Strict (State, execState, get, put, modify)
import Control.Exception (SomeException)
import Control.Monad (join, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_, foldr', toList)
import Data.Function (fix)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import Data.Maybe (fromJust)
import Data.MultiHashMap.Set.Strict (MultiHashMap)
import qualified Data.MultiHashMap.Set.Strict as MHM
import Data.Poly.Strict (Poly)
import Data.Sequence (Seq((:<|)))
import qualified Data.Sequence as S
import Data.Sequence.Unicode ((⊲), (⊳), (⋈))
import Data.Unique (Unique, newUnique)
import Game.AcidRain.World
  ( Command(..), CommandType(..), CommandID
  , CommandSetUpdated(..)
  , IClientCtx(..), ClientCtx )
import Game.AcidRain.World.Player (PlayerID, PlayerMoved(..))
import Game.AcidRain.World.Position (wpX, wpY)
import Game.AcidRain.TUI.AppEvent (AppEvent(..))
import Game.AcidRain.TUI.Keystroke (Keystroke, keystroke)
import Game.AcidRain.TUI.Widgets.WorldView
  ( WorldView, wvWorld, wvPlayer, worldView
  , renderWorldView, redrawWorldView, updatePlayerOffset )
import Game.AcidRain.World
  ( World(..), WorldState(..), WorldStateChanged(..) )
import qualified Game.AcidRain.TUI.Window as W
import qualified Game.AcidRain.World.Event as WE
import qualified Graphics.Vty as V
import Lens.Micro ((^.), (.~), (&), (%~), to, traverseOf)
import Lens.Micro.TH (makeLenses)
import Prelude.Unicode ((∘), (≡), (≢))


-- | The purpose of the data type 'Client' is to render and handle
-- events for a 'World'. It translates Vty input events into game
-- commands and sends them to the 'World'. It also handles world
-- events and updates Brick widgets accordingly.
data Client
  = Client
    { _cliEvChan       ∷ !(BChan (Poly AppEvent))
    , _cliWorldView    ∷ !(WorldView Unique)
      -- | Invariant: no 'HUD' windows come before 'Modal'.
    , _cliWindows      ∷ !(Seq ShownWindow)
      -- | The latest 'WorldState' we have observed.
    , _cliWorldState   ∷ !(WorldState ())
    , _cliAllCommands  ∷ !(HashSet (Poly Command))
    , _cliKeyMap       ∷ !(MultiHashMap Keystroke (Poly Command))
    , _cliEvDispatcher ∷ !(WE.EventDispatcher () (Eff '[State Client, Lift (EventM Unique)]))
    , _cliClosed       ∷ !Bool
    }

data ShownWindow
  = ShownWindow
    { _swWindow  ∷ !(Poly W.Window)
      -- | Becomes 'True' after invoking 'W.windowStartEvent'.
    , _swStarted ∷ !Bool
    }

makeLenses ''Client
makeLenses ''ShownWindow

instance Named ShownWindow Unique where
  getName = getName ∘ (^.swWindow)

instance IClientCtx Client where
  basicGetClientWorld = (^.cliWorldView.wvWorld)
  basicGetClientPlayerID = (^.cliWorldView.wvPlayer)
  basicHasWindow wid = any ((≡ wid) ∘ (^.swWindow.to W.windowID)) ∘ (^.cliWindows)
  basicInsertWindow win cli
    = do let sWin = ShownWindow (W.upcastWindow win) False
             cli' = case W.windowType win of
                      W.HUD   → cli & cliWindows %~ insHUD sWin
                      W.Modal → cli & cliWindows %~ (sWin ⊲)
         liftIO $ writeBChan (cli^.cliEvChan) (upcastAppEvent $ WindowInserted $ getName sWin)
         return cli'
    where
      insHUD ∷ ShownWindow → Seq ShownWindow → Seq ShownWindow
      insHUD sWin ws
        = let (modals, huds) = S.spanl ((≡ W.HUD) ∘ (^.swWindow.to W.windowType)) ws
          in
            modals ⋈ (sWin ⊲ huds)
  basicDeleteWindow wid
    = cliWindows %~ S.filter ((≢ wid) ∘ (^.swWindow.to W.windowID))

data ClientEvent
  = -- | Needed to redraw the client for whatever reason.
    RedrawNeeded
    -- | A world event has been fired.
  | GotWorldEvent !(Poly WE.Event)
    -- | A new 'Window' with the given name has been inserted.
  | WindowInserted !Unique
  deriving AppEvent

-- | Create a 'Client' interacting with a given 'World'.
newClient ∷ (World w, MonadIO μ)
          ⇒ Bool     -- ^ Whether to use Unicode characters
          → w        -- ^ The world to interact
          → PlayerID -- ^ The player to interact with the world
          → BChan (Poly AppEvent) -- ^ Brick application event channel
          → μ Client
newClient uni w pid evChan
  = do name ← liftIO newUnique

       -- We need to redirect world events to the application event
       -- channel, which needs a separate thread.
       void $ liftIO $ forkIO $ fix $ \loop →
         do mwEv ← waitForEvent w
            case mwEv of
              Just wEv → writeBChan evChan (upcastAppEvent $ GotWorldEvent wEv) *> loop
              Nothing  → return ()

       -- We need to send an event to the channel just to redraw the
       -- client, because it's initially empty due to the way how we
       -- render it, i.e. the extent thingy.
       liftIO $ writeBChan evChan (upcastAppEvent RedrawNeeded)

       ws ← getWorldState w
       return Client
         { _cliEvChan       = evChan
         , _cliWorldView    = worldView name uni w pid
         , _cliWindows      = mempty
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
  = let wins = join $ fmap renderWindow (cli^.cliWindows)
        wv   = renderWorldView (cli^.cliWorldView)
    in
      toList (wins ⊳ wv)
  where
    renderWindow ∷ ShownWindow → Seq (Widget Unique)
    renderWindow sWin
      = if sWin^.swStarted then
          W.renderWindow (sWin^.swWindow)
        else
          S.empty

isClientClosed ∷ Client → Bool
isClientClosed = (^.cliClosed)

handleClientEvent ∷ Client → BrickEvent Unique (Poly AppEvent) → EventM Unique Client
handleClientEvent cli be
  = case be of
      VtyEvent (V.EvResize _ _) → redraw

      AppEvent ev
        | Just RedrawNeeded ← downcastAppEvent ev
          → redraw

      _ →
        case cli^.cliWorldState of
          Loading         → handleEventWhileLoading cli be
          LoadPending     → fail "FIXME: not implemented"
          LoadFailed _    → fail "FIXME: not implemented"
          Running _       → handleEventWhileRunning cli be
          Closed (Just _) → fail "FIXME: not implemented"
          Closed Nothing  → return $ cli & cliClosed .~ True
  where
    redraw =
      traverseOf cliWorldView redrawWorldView cli

handleEventWhileLoading ∷ Client
                        → BrickEvent Unique (Poly AppEvent)
                        → EventM Unique Client
handleEventWhileLoading cli be
  = case be of
      AppEvent ev
        | Just (GotWorldEvent we) ← downcastAppEvent ev
          → handleWorldEvent cli we

      VtyEvent (V.EvKey V.KEsc []) →
        return $ cli & cliClosed .~ True

      _ → return cli

handleEventWhileRunning ∷ Client
                        → BrickEvent Unique (Poly AppEvent)
                        → EventM Unique Client
handleEventWhileRunning cli be
  = case be of
      AppEvent ev
        | Just (GotWorldEvent we) ← downcastAppEvent ev
          → -- World events are always propagated through all the windows
            -- and the client itself.
            do let handleWE = traverseOf swWindow (flip W.handleWorldEvent we)
               cli' ← traverseOf cliWindows (traverse handleWE) cli
               handleWorldEvent cli' we

        | Just (WindowInserted n) ← downcastAppEvent ev
          → traverseOf cliWindows (windowStart n)cli

      VtyEvent ev →
        -- Vty events are propagated through modal windows and finally
        -- to the client itself.
        do (ws', pr) ← propagateVtyEvent ev (cli^.cliWindows)
           let cli' = cli & cliWindows .~ ws'
           if pr
             then handleVtyEventWhileRunning cli' ev
             else return cli'

      _ → return cli

  where
    windowStart ∷ Unique → Seq ShownWindow → EventM Unique (Seq ShownWindow)
    windowStart n = go S.empty
      where
        go ∷ Seq ShownWindow → Seq ShownWindow → EventM Unique (Seq ShownWindow)
        go done S.Empty      = return done
        go done (sw :<| sws) = if getName sw ≡ n then
                                 do sw' ← traverseOf swWindow W.windowStartEvent sw
                                    return $ (done ⊳ (sw' & swStarted .~ True)) ⋈ sws
                               else
                                 go (done ⊳ sw) sws

    propagateVtyEvent ∷ V.Event → Seq ShownWindow → EventM Unique (Seq ShownWindow, Bool)
    propagateVtyEvent ev = go True S.empty
      where
        go ∷ Bool → Seq ShownWindow → Seq ShownWindow → EventM Unique (Seq ShownWindow, Bool)
        go False done rest         = return (rest ⋈ done, False)
        go True  done S.Empty      = return (done, True)
        go True  done (sw :<| sws)
          = case W.windowType (sw^.swWindow) of
              W.Modal →
                do (w', pr) ← W.handleVtyEvent (sw^.swWindow) ev
                   go pr (done ⊳ (sw & swWindow .~ w')) sws
              W.HUD →
                return (done ⋈ (sw ⊲ sws), True)

handleVtyEventWhileRunning ∷ Client → V.Event → EventM Unique Client
handleVtyEventWhileRunning cli ev
  = case ev of
      V.EvKey V.KEsc [] →
        -- FIXME: Show pause screen
        return $ cli & cliClosed .~ True

      V.EvKey key mods →
        do let ks   = keystroke key mods
               cmds = MHM.lookup ks (cli^.cliKeyMap)
           withClientCtx cli $
             traverse_ (flip runOnClient []) cmds

      _ →
        return cli

withClientCtx ∷ MonadIO μ
              ⇒ Client
              → Eff '[State ClientCtx, Exc SomeException, Lift μ] ()
              → μ Client
withClientCtx cli m
  = do eRes ← runLift $ runError $ execState (upcastClientCtx cli) $ m
       ctx  ← handleCmdExc eRes
       return $ fromJust $ downcastClientCtx ctx
  where
    -- FIXME: Report it to the player without crashing the client.
    handleCmdExc ∷ MonadIO μ ⇒ Either SomeException a → μ a
    handleCmdExc (Left  e) = error ("FIXME: command failed: " ++ show e)
    handleCmdExc (Right a) = return a

handleWorldEvent ∷ Client → Poly WE.Event → EventM Unique Client
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

defaultKeyMap ∷ HashSet (Poly Command) → MultiHashMap Keystroke (Poly Command)
defaultKeyMap = foldr' f MHM.empty
  where
    f cmd keyMap
      = case commandType cmd of
          Interactive (Just k) → MHM.insert k cmd keyMap
          _                    → keyMap

-- | Update a key map. Old key mappings will be retained iff their
-- commands also exist in the new one.
updateKeyMap ∷ MultiHashMap Keystroke (Poly Command)
             → MultiHashMap Keystroke (Poly Command)
             → MultiHashMap Keystroke (Poly Command)
updateKeyMap old new = MHM.foldrWithKey' f MHM.empty new
  where
    f ∷ Keystroke
      → Poly Command
      → MultiHashMap Keystroke (Poly Command)
      → MultiHashMap Keystroke (Poly Command)
    f newK cmd km
      = case HM.lookup (commandID cmd) revOld of
          Just oldK → MHM.insert oldK cmd km
          Nothing   → MHM.insert newK cmd km

    revOld ∷ HashMap CommandID Keystroke
    revOld = MHM.foldrWithKey' g HM.empty old

    g ∷ Keystroke
      → Poly Command
      → HashMap CommandID Keystroke
      → HashMap CommandID Keystroke
    g k cmd = HM.insert (commandID cmd) k
