{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Direction
  ( Direction(..)
  ) where


data Direction
  = North
  | West
  | East
  | Sourth
  | Up
  | Down
  deriving (Show, Eq)
