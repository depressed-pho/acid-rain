{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.TUI.Widgets.WorldView
  ( WorldView, wvWorld, wvPlayer
  , worldView
  , renderWorldView
  , redrawWorldView
  , updatePlayerOffset
  ) where

import Brick.Main (lookupExtent)
import Brick.Types
  ( Location(..), Widget(..), Size(..), EventM, Extent(..)
  , availWidthL, availHeightL, emptyResult, getContext, imageL, locL )
import Brick.Widgets.Center (center, hCenter, vCenter)
import Brick.Widgets.Core (Named(..), fill, reportExtent, txt, txtWrap, vBox)
import Control.Exception (Handler(..), catches)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Convertible.Base (convert)
import Data.Int (Int32)
import Data.Monoid.Unicode ((⊕))
import Data.Poly.Strict (Poly)
import Data.Text (pack)
import Game.AcidRain.TUI (Appearance(..), HasAppearance(..))
import Game.AcidRain.World
  ( World(..), WorldState(..), WorldNotRunningException(..) )
import Game.AcidRain.World.Chunk (Chunk, chunkHeight, entityAt, tileStateAt)
import Game.AcidRain.World.Chunk.Position (ChunkPos(..), cpX, cpY, toWorldPos)
import Game.AcidRain.World.Player (PlayerID, plPos)
import Game.AcidRain.World.Position (WorldPos(..), wpX, wpY, wpZ, lowestZ)
import qualified Graphics.Vty as V
import Lens.Micro (Getting, (&), (.~), (^.), (+~), (-~), (%~), _1, _2, to)
import Lens.Micro.TH (makeLenses)
import Prelude.Unicode ((≤), (≥))


-- | This is a Brick widget to render some part of the world centered
-- around a tracked player. Since we cannot do any I/O in 'RenderM',
-- the 'V.Image' to be rendered is in fact reconstructed in
-- 'redrawWorldView' but not in 'renderWorldView'.
data WorldView n
  = WorldView
    { _wvName    ∷ !n
    , _wvUnicode ∷ !Bool
    , _wvWorld   ∷ !(Poly World)
      -- | The player to trac.
    , _wvPlayer  ∷ !PlayerID
      -- | The offset from the center of the widget where the player
      -- should be located.
    , _wvPlayerOffset ∷ !Location
      -- | The 'V.Image' to be rendered. It's 'Nothing' until it's
      -- once rendered.
    , _wvWidget  ∷ !(Maybe (Widget n))
    }

makeLenses ''WorldView

instance Named (WorldView n) n where
  getName = _wvName

-- | Create a world view.
worldView ∷ World w
          ⇒ n        -- ^ The name of this widget
          → Bool     -- ^ Whether to use Unicode characters
          → w        -- ^ The world to render
          → PlayerID -- ^ The player to track
          → WorldView n
worldView n uni w pid
  = WorldView
    { _wvName         = n
    , _wvUnicode      = uni
    , _wvWorld        = upcastWorld w
    , _wvPlayer       = pid
    , _wvPlayerOffset = Location (0, 0)
    , _wvWidget       = Nothing
    }

-- | Render a world view.
renderWorldView ∷ Ord n ⇒ WorldView n → Widget n
renderWorldView wv
  = reportExtent (wv^.wvName) $
    case wv^.wvWidget of
      Just w  → w
      Nothing → fill ' ' -- Take up all the available space.

-- | Redraw a world view.
redrawWorldView ∷ ∀n. Eq n ⇒ WorldView n → EventM n (WorldView n)
redrawWorldView wv
  = do mExt ← lookupExtent (wv^.wvName)
       w    ← traverse mkWidget mExt
       return $ wv & wvWidget .~ w
  where
    mkWidget ∷ Extent n → EventM n (Widget n)
    mkWidget ext
      = liftIO $
        -- Now we are in the IO monad.
        flip catches [ Handler catchWNRE
                     ] $
        do -- Draw all the tiles currently visible from the
           -- viewpoint. The easiest way to do this is to iterate on
           -- every visible world position and ask the world for the
           -- tile there, but that would cause too many chunk lookups
           -- and would be terribly inefficient. So we iterate on
           -- visible chunks instead, and render their visible parts.
           wTopLeft ← worldPosAt wv ext $ Location (0, 0)
           let wTopRight    = wTopLeft & wpX %~ \x → x + fromIntegral (ext^.to extentSize._1) - 1
               wBottomLeft  = wTopLeft & wpY %~ \y → y + fromIntegral (ext^.to extentSize._2) - 1
               cTopLeft     = convert wTopLeft
               cTopRight    = convert wTopRight
               cBottomLeft  = convert wBottomLeft
               cBottomRight = cBottomLeft & cpX .~ (cTopRight^.cpX)

           let cRow cy    = V.horizCat <$> mapM (flip cCol cy) [cTopLeft^.cpX .. cTopRight^.cpX]
               cCol cx cy = let cPos = ChunkPos cx cy
                            in renderChunk wTopLeft wTopRight wBottomLeft cPos
           cRows ← V.vertCat <$> mapM cRow [cTopLeft^.cpY .. cBottomLeft^.cpY]

           -- Subscribe to ChunkUpdated events for these chunks.
           subscribeToChunks (wv^.wvWorld) (wv^.wvPlayer) (cTopLeft, cBottomRight)

           -- The RenderM is required to fill up the entire space
           -- reported by availWidthL and availHeightL, but the size
           -- of extent doesn't necessarily match with it. So we need
           -- to resize the image we just constructed.
           return $
             Widget Greedy Greedy $
             do ctx ← getContext
                let img = V.resize (ctx^.availWidthL) (ctx^.availHeightL) cRows
                return $ emptyResult & imageL .~ img

    renderChunk ∷ (MonadThrow μ, MonadIO μ)
                ⇒ WorldPos
                → WorldPos
                → WorldPos
                → ChunkPos
                → μ V.Image
    renderChunk wTopLeft wTopRight wBottomLeft cPos
      = do chunk' ← lookupChunk (wv^.wvWorld) cPos

           -- Now the problem is how to determine the visible area of
           -- this chunk. For each chunk we know which area in the
           -- world coords the chunk covers.
           let wcTopLeft    = toWorldPos cPos 0
               wcTopRight   = (convert wcTopLeft) & cpX +~ 1 & flip toWorldPos 0 & wpX -~ 1
               wcBottomLeft = (convert wcTopLeft) & cpY +~ 1 & flip toWorldPos 0 & wpY -~ 1

               wyBegin = max (wTopLeft   ^.wpY) (wcTopLeft   ^.wpY)
               wyEnd   = min (wBottomLeft^.wpY) (wcBottomLeft^.wpY)
               wxBegin = max (wTopLeft   ^.wpX) (wcTopLeft   ^.wpX)
               wxEnd   = min (wTopRight  ^.wpX) (wcTopRight  ^.wpX)

           -- And we know which area in the world coords is visible.
           case chunk' of
             Nothing    → return $ V.charFill V.defAttr ' ' (wxEnd-wxBegin+1) (wyEnd-wyBegin+1)
             Just chunk →
               let wRow wy    = V.horizCat <$> mapM (flip wCol wy) [wxBegin .. wxEnd]
                   wCol wx wy = renderXY chunk wx wy
               in
                 V.vertCat <$> mapM wRow [wyBegin .. wyEnd]

    renderXY ∷ MonadThrow μ ⇒ Chunk → Int32 → Int32 → μ V.Image
    renderXY chunk wx wy = go (lowestZ + chunkHeight - 1)
      where
        -- Search for a visible object from the highest Z to the
        -- lowest. If nothing's found leave there blank.
        go wz = do let pos = WorldPos wx wy wz
                   mi ← renderAt chunk pos
                   case mi of
                     Just i                 → return i
                     Nothing | wz > lowestZ → go (wz-1)
                             | otherwise    → return blank
        blank ∷ V.Image
        blank = V.char V.defAttr ' '

    renderAt ∷ MonadThrow μ ⇒ Chunk → WorldPos → μ (Maybe V.Image)
    renderAt chunk pos
      -- If there is a visible entity here, then it has the highest
      -- priority.  Next an item pile, then a tile.
      = let off = convert pos
        in case renderAppr <$> entityAt off chunk of
             Just i  → return i
             Nothing →
               do ts ← tileStateAt off chunk
                  return $ renderAppr (ts, pos)

    renderAppr ∷ HasAppearance α ⇒ α → Maybe V.Image
    renderAppr a
      = case appearance a of
          VisibleAppearance { .. } →
            Just $
            if wv^.wvUnicode
            then V.text' vaAttr vaUnicode
            else V.char  vaAttr vaAscii
          InvisibleAppearance → Nothing

-- | We don't want to scroll the view each time a player moves. We
-- only do it when a player goes to proximities of the view
-- border. This function take that in account and only scroll the view
-- when necessary.
updatePlayerOffset ∷ ∀n. Eq n ⇒ Location → WorldView n → EventM n (WorldView n)
updatePlayerOffset δ wv
  = do mExt ← lookupExtent (wv^.wvName)
       case mExt of
         Just ext →
           do let off0 = (wv^.wvPlayerOffset) `addLoc` δ
                  off1 = off0 & locL._1 %~ constraint ext _1
                              & locL._2 %~ constraint ext _2
              return $ wv & wvPlayerOffset .~ off1
         Nothing →
           return wv
  where
    constraint ∷ Extent n → Getting Int (Int, Int) Int → Int → Int
    constraint ext compL comp
      = let extSize  = ext^.to extentSize.compL
            extBegin = negate (extSize `div` 2)
            extEnd   = extBegin + extSize
            comp'
              | comp ≤ extBegin + borderSize ext compL = 0
              | comp ≥ extEnd   - borderSize ext compL = 0
              | otherwise                              = comp
        in comp'

    borderSize ∷ Extent n → Getting Int (Int, Int) Int → Int
    borderSize ext compL
      = min 1 $ (ext^.to extentSize.compL) `div` 5

-- Convert a point in the local coords to that of the world coords.
worldPosAt ∷ MonadIO μ
           ⇒ WorldView n
           → Extent n
           → Location
           → μ WorldPos
worldPosAt wv ext lp
  = do -- Get the player position
       pl ← getPlayerFromWorld (wv^.wvWorld) (wv^.wvPlayer)
       let pposW  = pl^.plPos

       -- Get the player position in the widget coords.
       let centerL = getCenter ext
           pposL   = centerL `addLoc` (wv^.wvPlayerOffset)

       -- Now that we have these, we know how these coords correspond
       -- to each other.
       let δx = pposW^.wpX - fromIntegral (pposL^.locL._1)
           δy = pposW^.wpY - fromIntegral (pposL^.locL._2)
       return $ WorldPos
         { _wpX = fromIntegral (lp^.locL._1) + δx
         , _wpY = fromIntegral (lp^.locL._2) + δy
         , _wpZ = pposW^.wpZ
         }

getCenter ∷ Extent n → Location
getCenter ext
  = Location $
    (ext^.to extentSize) & _1 %~ (`div` 2)
                         & _2 %~ (`div` 2)

addLoc ∷ Location → Location → Location
addLoc a b
  = a & locL._1 +~ b^._1
      & locL._2 +~ b^._2

catchWNRE ∷ WorldNotRunningException → IO (Widget n)
catchWNRE (WorldNotRunningException ws)
  = return $
    case ws of
      Loading       → vCenter $ vBox [ hCenter $ txt "Loading..."
                                     , txt ""
                                     , hCenter $ txt "Press ESC to cancel" ]
      LoadPending   → center $ txt "FIXME: LoadPending"
      LoadFailed e' → txtWrap $ "Load failed: " ⊕ pack (show e')
      Running _     → error "Impossible"
      Closed e'     → txtWrap $ "World closed: " ⊕ pack (show e')
