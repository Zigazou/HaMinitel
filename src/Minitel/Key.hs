{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Key
Description : Abstraction of the Minitel keys
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module provides an abstraction to the Minitel keys.
-}
module Minitel.Key
( Modifier(Plain, Shift, Ctrl, CtrlShift)
, Key( KeyAlpha, KeyTOC, KeyCancel, KeyPrev, KeyRepeat, KeyGuide, KeyCorrection
     , KeyNext, KeySend, KeyConnection, KeyReturn, KeyEsc, KeyLeft, KeyRight
     , KeyUp, KeyDown, KeyCedilla, KeyGrave, KeyAcute, KeyCircumflex
     , KeyDiaeresis
     )
, toAlpha
, toKey
)
where

import Minitel.Constants.Accents
import qualified Minitel.Constants.Keyboard as Key
import qualified Minitel.Constants.ASCII    as ASCII
import Minitel.Type.MNatural (chrm)
import Minitel.Type.MString (MString)
import Data.Maybe (isJust, fromJust)

-- | Every possible character of the Minitel
minitelChars :: String
minitelChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\
               \äëïöüâêîôûàèùéçœŒ{}*$!:;,?./\\&(-_)=+'@0123456789 \
               \↑←→↓°÷±§"

isMinitelChar :: Char -> Bool
isMinitelChar c = c `elem` minitelChars

-- | The Minitel has 3 type of modifier : Shift, Ctrl or Ctrl+Shift
data Modifier = Plain | Shift | Ctrl | CtrlShift deriving (Eq, Show)

-- | Every possible key on the Minitel keyboard (visible + functions)
data Key = KeyAlpha      Char
         | KeyTOC        Modifier
         | KeyCancel     Modifier
         | KeyPrev       Modifier
         | KeyRepeat     Modifier
         | KeyGuide      Modifier
         | KeyCorrection Modifier
         | KeyNext       Modifier
         | KeySend       Modifier
         | KeyConnection Modifier
         | KeyReturn     Modifier
         | KeyEsc        Modifier
         | KeyLeft       Modifier
         | KeyRight      Modifier
         | KeyUp         Modifier
         | KeyDown       Modifier
         | KeyCedilla    Modifier
         | KeyGrave      Modifier
         | KeyAcute      Modifier
         | KeyCircumflex Modifier
         | KeyDiaeresis  Modifier
         deriving (Eq, Show)

-- | Converts a key sequence to a Unicode character
toAlpha :: MString -> Maybe Char
toAlpha [] = Nothing
toAlpha (AddCedilla    ASCII.LowerC) = Just 'ç'
toAlpha (AddCedilla    ASCII.UpperC) = Just 'Ç'
toAlpha (AddGrave      ASCII.LowerA) = Just 'à'
toAlpha (AddCircumflex ASCII.LowerA) = Just 'â'
toAlpha (AddUmlaut     ASCII.LowerA) = Just 'ä'
toAlpha (AddGrave      ASCII.LowerE) = Just 'è'
toAlpha (AddAcute      ASCII.LowerE) = Just 'é'
toAlpha (AddCircumflex ASCII.LowerE) = Just 'ê'
toAlpha (AddUmlaut     ASCII.LowerE) = Just 'ë'
toAlpha (AddCircumflex ASCII.LowerI) = Just 'î'
toAlpha (AddUmlaut     ASCII.LowerI) = Just 'ï'
toAlpha (AddCircumflex ASCII.LowerO) = Just 'ô'
toAlpha (AddUmlaut     ASCII.LowerO) = Just 'ö'
toAlpha (AddGrave      ASCII.LowerU) = Just 'ù'
toAlpha (AddCircumflex ASCII.LowerU) = Just 'û'
toAlpha (AddUmlaut     ASCII.LowerU) = Just 'ü'
toAlpha (AddCedilla    mn          ) = Just $ chrm mn
toAlpha (AddCircumflex mn          ) = Just $ chrm mn
toAlpha (AddUmlaut     mn          ) = Just $ chrm mn
toAlpha (AddAcute      mn          ) = Just $ chrm mn
toAlpha (AddGrave      mn          ) = Just $ chrm mn
toAlpha s
    | isMinitelChar fstC            = Just fstC
    | otherwise                     = Nothing
    where fstC = (chrm . head) s

-- | Converts an MString to a key
toKey :: MString -> Key
toKey []                 = error "No key"
toKey Cedilla            = KeyCedilla Plain
toKey Grave              = KeyGrave Plain
toKey Acute              = KeyAcute Plain
toKey Circumflex         = KeyCircumflex Plain
toKey Umlaut             = KeyDiaeresis Plain
toKey Key.Up             = KeyUp Plain
toKey Key.Down           = KeyDown Plain
toKey Key.Left           = KeyLeft Plain
toKey Key.Right          = KeyRight Plain
toKey Key.ShiftUp        = KeyUp Shift
toKey Key.ShiftDown      = KeyDown Shift
toKey Key.ShiftLeft      = KeyLeft Shift
toKey Key.ShiftRight     = KeyRight Shift
toKey Key.CtrlLeft       = KeyLeft Ctrl
toKey Key.Return         = KeyReturn Plain
toKey Key.ShiftReturn    = KeyReturn Shift
toKey Key.CtrlReturn     = KeyReturn Ctrl
toKey Key.FuncSend       = KeySend Plain
toKey Key.FuncPrev       = KeyPrev Plain
toKey Key.FuncRepeat     = KeyRepeat Plain
toKey Key.FuncGuide      = KeyGuide Plain
toKey Key.FuncCancel     = KeyCancel Plain
toKey Key.FuncTOC        = KeyTOC Plain
toKey Key.FuncCorrection = KeyCorrection Plain
toKey Key.FuncNext       = KeyNext Plain
toKey Key.FuncConnection = KeyConnection Plain
toKey s
    | isJust alpha           = KeyAlpha (fromJust alpha)
    | otherwise              = error $ "Unsupported key: " ++ show s
    where alpha = toAlpha s

