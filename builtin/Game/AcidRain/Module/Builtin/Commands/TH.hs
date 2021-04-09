{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Commands.TH
  ( mkWalkCmd
  ) where

import Control.Monad (void)
import Data.Proxy (Proxy)
import Game.AcidRain.TUI.Keystroke (Keystroke)
import Game.AcidRain.World
  ( Command(..), CommandType(..), CommandID, BadArgumentsException(..)
  , throwSomeExc, getPlayer, getPlayer, tryMoveEntity )
import Game.AcidRain.World.Player (plPos)
import Language.Haskell.TH.Lib (dataD, instanceD, funD, clause, wildP, normalB, varP, varE, appE, conE, parensE, doE, bindS, noBindS, uInfixE, sectionR)
import Language.Haskell.TH.Syntax (Q, Lift(..), Dec, Exp, Type(..), newName, mkName)
import Language.Haskell.TH.Syntax.Compat (IsCode(..), unTypeCode)
import Lens.Micro ((^.), (&), (<&>))


mkWalkCmd ∷ String    -- ^ type name
          → CommandID -- ^ command ID
          → Keystroke -- ^ default key binding
          → Q Exp     -- ^ movement ('WorldPos' → 'WorldPos')
          → Q [Dec]
mkWalkCmd tyName cmdID ks move
  = do -- data {tyName}
       d    ← dataD (return []) (mkName tyName) [] Nothing [] []
       -- instance Command (Proxy {tyName}) where
       let proxTy = return $ ConT ''Command `AppT` ParensT (ConT ''Proxy `AppT` ConT (mkName tyName))
       pidN ← newName "pid"
       posN ← newName "pos"
       i    ← instanceD (return []) proxTy
              [ -- commandID _ = {cmdID}
                funD 'commandID
                [ clause [wildP]
                  ( normalB $
                    lift cmdID
                  )
                  []
                ]
                -- commandType _ = Interactive (Just {ks})
              , funD 'commandType
                [ clause [wildP]
                  ( normalB $ unTypeCode $ toCode
                    [|| Interactive (Just ks) ||]
                  )
                  []
                ]
              , funD 'runOnWorld
                [ -- runOnWorld _ (Just pid) [] = do ...
                  clause [wildP, [p| Just $(varP pidN) |], [p| [] |]]
                  ( normalB $ doE
                    [ -- do pos ← getPlayer pid <&> (^.plPos)
                      bindS (varP posN)
                      ( uInfixE
                        (varE 'getPlayer `appE` varE pidN)
                        (varE '(<&>))
                        (sectionR (varE '(^.)) (varE 'plPos))
                      )
                      --    void (tryMoveEntity pos (pos & {move}))
                    , noBindS
                      ( varE 'void `appE`
                        parensE ( varE 'tryMoveEntity `appE`
                                  varE posN `appE`
                                  parensE (uInfixE (varE posN) (varE '(&)) move) )
                      )
                    ]
                  )
                  []
                  -- runOnWorld _ _ _ = throwSomeExc BadArgumentsException
                , clause [wildP, wildP, wildP]
                  ( normalB (varE 'throwSomeExc `appE` conE 'BadArgumentsException)
                  )
                  []
                ]
           ]
       return [d, i]
