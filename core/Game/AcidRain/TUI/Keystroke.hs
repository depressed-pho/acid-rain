{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.TUI.Keystroke
  ( Keystroke(..)
  , keystroke
  , keyQ
  ) where

import Control.Monad (void)
import Data.Char (isLetter, isNumber, isPunctuation, isSymbol, ord)
import Data.Hashable (Hashable(..))
import Data.Hashable.Generic (genericHashWithSalt)
import Data.Monoid.Unicode ((⊕))
import Data.Set (Set)
import qualified Data.Set as S
import Graphics.Vty.Input.Events (Key(..), Modifier(..))
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Lift.Generics (genericLift)
import Language.Haskell.TH.Lift.Set (genericLiftSet)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Lift(..))
import Numeric.Natural (Natural)
import Prelude hiding (mod)
import Prelude.Unicode ((∘), (∨), (⋅))
import Text.Parsec (ParsecT, Stream, (<|>), choice, eof, many, parse, try)
import Text.Parsec.Char (char, digit, satisfy, spaces, string)


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

instance Lift Keystroke where
  lift ks
    = [| Keystroke
         { ksKey  = $(genericLift    $ ksKey  ks)
         , ksMods = $(genericLiftSet $ ksMods ks)
         }
       |]

keystroke ∷ Key → [Modifier] → Keystroke
keystroke key mods
  = Keystroke key $ S.fromList mods

-- | Quasi-quotation @['keyQ'| ... |]@ can generate an expression of
-- type 'Keystroke' by parsing a keystroke represented in a special
-- syntax.
--
-- FIXME: document the syntax
keyQ ∷ QuasiQuoter
keyQ = QuasiQuoter
       { quoteExp  = (lift =<<) ∘ parseKey
       , quotePat  = const unsupported
       , quoteType = const unsupported
       , quoteDec  = const unsupported
       }
  where
    unsupported ∷ Q α
    unsupported = fail "The quatation [keyQ| .. |] can only be used for expressions"

parseKey ∷ String → Q Keystroke
parseKey str
  = case parse properKeystroke "" str of
      Left err → fail $ show err
      Right ks → return ks
  where
    properKeystroke
      = do spaces
           ks ← keystrokeP
           spaces
           eof
           return ks

keystrokeP ∷ Stream s m Char ⇒ ParsecT s () m Keystroke
keystrokeP
  = do mods ← nonDupingSet =<< many (try modifierP)
       key  ← keyP
       return $ Keystroke key mods

nonDupingSet ∷ (Ord a, Show a) ⇒ [a] → ParsecT s u m (Set a)
nonDupingSet = go S.empty
  where
    go s []            = return s
    go s (x:xs)
      | x `S.member` s = fail ("Duplicate element: " ⊕ show s)
      | otherwise      = go (S.insert x s) xs

keyP ∷ Stream s m Char ⇒ ParsecT s () m Key
keyP = try specialKeyP <|> regularKeyP

specialKeyP ∷ Stream s m Char ⇒ ParsecT s () m Key
specialKeyP
  = do void $ char '<'
       key ← choice
             [ string "tab"       *> return (KChar '\t')
             , string "escape"    *> return KEsc
             , string "backspace" *> return KBS
             , string "return"    *> return KEnter
             , string "left"      *> return KLeft
             , string "right"     *> return KRight
             , string "up"        *> return KUp
             , string "down"      *> return KDown
             , string "print"     *> return KPrtScr
             , string "pause"     *> return KPause
             , string "insert"    *> return KIns
             , string "home"      *> return KHome
             , string "prior"     *> return KPageUp
             , string "delete"    *> return KDel
             , string "end"       *> return KEnd
             , string "next"      *> return KPageDown
             , do void $ char 'f'
                  n ← natural
                  return $ KFun $ fromIntegral n
             ]
       void $ char '>'
       return key
    <|>
    choice
    [ string "SPC" *> return (KChar ' ')
    , string "TAB" *> return (KChar '\t')
    , string "ESC" *> return KEsc
    , string "DEL" *> return KBS
    , string "RET" *> return KEnter
    ]

natural ∷ Stream s m Char ⇒ ParsecT s () m Natural
natural = go 0
  where
    go n = do d ← digit
              go (n⋅10 + fromIntegral (ord d - ord '0'))
           <|>
           return n

regularKeyP ∷ Stream s m Char ⇒ ParsecT s () m Key
regularKeyP
  = do c ← satisfy $ \c → isLetter c ∨ isNumber c ∨ isPunctuation c ∨ isSymbol c
       return $ KChar c

modifierP ∷ Stream s m Char ⇒ ParsecT s () m Modifier
modifierP
  = do mod ← choice
             [ char 'S' *> return MShift
             , char 'C' *> return MCtrl
             , char 'M' *> return MMeta
             , char 'A' *> return MAlt
             ]
       void $ char '-'
       return mod
