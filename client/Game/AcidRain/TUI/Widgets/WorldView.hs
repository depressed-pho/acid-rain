{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.TUI.Widgets.WorldView
  ( WorldView
  , worldView
  , renderWorldView
  ) where

import Brick.Types
  ( Location(..), Widget(..), Size(..), Context, Result, RenderM
  , getContext, emptyResult, imageL, availWidthL, availHeightL, locL )
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (Named(..), txtWrap)
import Control.Exception (Handler(..), SomeException, catches)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Convertible.Base (convert)
import Data.Monoid.Unicode ((⊕))
import Data.Text (pack)
import Game.AcidRain.TUI (Appearance(..))
import Game.AcidRain.World
  ( World(..), WorldState(..), SomeWorld, WorldNotRunningException(..) )
import Game.AcidRain.World.Chunk (Chunk, tileStateAt)
import Game.AcidRain.World.Chunk.Position (ChunkPos(..), cpX, cpY, toWorldPos)
import Game.AcidRain.World.Player (PlayerID, plPos)
import Game.AcidRain.World.Position (WorldPos(..), wpX, wpY, wpZ)
import Game.AcidRain.World.Tile (Tile(..), TileState(..))
import qualified Graphics.Vty as V
import Lens.Micro ((&), (.~), (^.), (+~), (-~), (%~), _1, _2)
import Prelude.Unicode ((∘))
import System.IO.Unsafe (unsafePerformIO)


data WorldView n
  = WorldView
    { wvName    ∷ !n
    , wvUnicode ∷ !Bool
    , wvWorld   ∷ !SomeWorld
      -- | The player to trac.
    , wvPlayer  ∷ !PlayerID
      -- | The offset from the center of the widget where the player
      -- should be located.
    , wvPlayerOffset ∷ !Location
    }

instance Named (WorldView n) n where
  getName = wvName

worldView ∷ World w ⇒ n → Bool → w → PlayerID → WorldView n
worldView n uni w pid
  = WorldView
    { wvName         = n
    , wvUnicode      = uni
    , wvWorld        = upcastWorld w
    , wvPlayer       = pid
    , wvPlayerOffset = Location (0, 0)
    }

renderWorldView ∷ ∀n. WorldView n → Widget n
renderWorldView wv
  = Widget Greedy Greedy $
    do ctx ← getContext
       unsafePerformIO $ render' ctx
  where
    -- RenderM currently isn't a MonadIO so we have to resort to
    -- unsafePerformIO. This is actually quite unsafe but we have no
    -- options.
    render' ∷ Context → IO (RenderM n (Result n))
    render' ctx
      = flip catches [ Handler catchAll
                     , Handler catchWNRE
                     ] $
        do -- Draw all the tiles currently visible from the
           -- viewpoint. The easiest way to do this is to iterate on
           -- every visible world position and ask the world for the
           -- tile there, but that would cause too many chunk lookups
           -- and would be terribly inefficient. So we iterate on
           -- visible chunks instead, and render their visible parts.
           wTopLeft ← worldPosAt wv ctx $ Location (0, 0)
           let wTopRight   = wTopLeft & wpX %~ \x → x + fromIntegral (ctx^.availWidthL ) - 1
               wBottomLeft = wTopLeft & wpY %~ \y → y + fromIntegral (ctx^.availHeightL) - 1
               cTopLeft    = convert wTopLeft
               cTopRight   = convert wTopRight
               cBottomLeft = convert wBottomLeft

           let cRow cy    = V.horizCat <$> mapM (flip cCol cy) [cTopLeft^.cpX .. cTopRight^.cpX]
               cCol cx cy = let cPos = ChunkPos cx cy
                            in renderChunk wTopLeft wTopRight wBottomLeft cPos
           cRows ← V.vertCat <$> mapM cRow [cTopLeft^.cpY .. cBottomLeft^.cpY]

           return $ return $ emptyResult & imageL .~ cRows

    renderChunk ∷ (MonadThrow μ, MonadIO μ) ⇒ WorldPos → WorldPos → WorldPos → ChunkPos → μ V.Image
    renderChunk wTopLeft wTopRight wBottomLeft cPos
      = do ensureChunkExists (wvWorld wv) cPos
           chunk' ← lookupChunk (wvWorld wv) cPos

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
             Nothing    → return $ V.charFill V.defAttr ' ' (wxEnd - wxBegin) (wyEnd - wyBegin)
             Just chunk →
               let wRow wy    = V.horizCat <$> mapM (flip wCol wy) [wxBegin .. wxEnd]
                   wCol wx wy = renderAt chunk $ WorldPos wx wy 0 -- FIXME: Z
               in
                 V.vertCat <$> mapM wRow [wyBegin .. wyEnd]

    renderAt ∷ MonadThrow μ ⇒ Chunk → WorldPos → μ V.Image
    renderAt chunk wpos
      = do ts ← tileStateAt chunk (convert wpos)
           let appr = appearance (tsTile ts) (tsValue ts)
           return $ if wvUnicode wv
                    then V.text' (apAttr appr) (apUnicode appr)
                    else V.char  (apAttr appr) (apAscii   appr)


-- Convert a point in the local coords to that of the world coords.
worldPosAt ∷ WorldView n → Context → Location → IO WorldPos
worldPosAt wv ctx lp
  = do -- Get the player position
       pl ← getPlayer (wvWorld wv) (wvPlayer wv)
       let pposW  = plPos pl

       -- Get the player position in the widget coords.
       let centerL = getCenter ctx
           pposL   = centerL `addLoc` wvPlayerOffset wv

       -- Now that we have these, we know how these coords correspond
       -- to each other.
       let δx = pposW^.wpX - fromIntegral (pposL^.locL._1)
           δy = pposW^.wpY - fromIntegral (pposL^.locL._2)
       return $ WorldPos
         { _wpX = fromIntegral (lp^.locL._1) + δx
         , _wpY = fromIntegral (lp^.locL._2) + δy
         , _wpZ = pposW^.wpZ
         }

getCenter ∷ Context → Location
getCenter ctx
  = let x = (ctx^.availWidthL ) `div` 2
        y = (ctx^.availHeightL) `div` 2
    in
      Location (x, y)

addLoc ∷ Location → Location → Location
addLoc a b
  = a & locL._1 +~ b^._1
      & locL._2 +~ b^._2

catchAll ∷ SomeException → IO (RenderM n (Result n))
catchAll = return ∘ render ∘ txtWrap ∘ pack ∘ show

catchWNRE ∷ WorldNotRunningException → IO (RenderM n (Result n))
catchWNRE (WorldNotRunningException ws)
  = let w = case ws of
              Loading      → center $ txtWrap "Loading..."
              LoadPending  → center $ txtWrap "FIXME: LoadPending"
              LoadFailed e → center $ txtWrap $ "Load failed: " ⊕ pack (show e)
              Running _    → error "Impossible"
              Closed e     → center $ txtWrap $ "World closed: " ⊕ pack (show e)
    in
      return $ render w
