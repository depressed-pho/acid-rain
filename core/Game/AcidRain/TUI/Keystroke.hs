{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.TUI.Keystroke
  ( Keystroke(..)
  ) where

import Data.Hashable (Hashable(..))
import Data.Hashable.Generic (genericHashWithSalt)
import Data.Set (Set)
import qualified Data.Set as S
import Graphics.Vty.Input.Events (Key, Modifier)


data Keystroke
  = Keystroke
    { ksKey  ∷ !Key
    , ksMods ∷ !(Set Modifier)
    }
  deriving (Show, Eq)

instance Hashable Keystroke where
  hashWithSalt salt ks
    = genericHashWithSalt salt (ksKey ks)
      `hashWithSalt`
      hashMods (ksMods ks)
    where
      hashMods ∷ Set Modifier → Int
      hashMods mods
        = S.foldl' genericHashWithSalt salt mods
