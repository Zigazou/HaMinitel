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
module Minitel.Key where

import Minitel.Constants.Constants
import Minitel.Type.MString
import Data.Char
import Data.Maybe (isJust, fromJust)

-- | Every possible character of the Minitel
minitelChars :: String
minitelChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\
               \äëïöüâêîôûàèùéçœŒ{}*$!:;,?./\\&(-_)=+'@0123456789 \
               \↑←→↓°÷±§"

isMinitelChar :: Char -> Bool
isMinitelChar c = c `elem` minitelChars

-- | The Minitel has 3 type of modifier : Shift, Ctrl or Ctrl+Shift
data Modifier = Plain | Shift | Ctrl | CtrlShift

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

-- | Converts a key sequence to a Unicode character
toAlpha :: MString -> Maybe Char
toAlpha s
    | isMinitelChar c               = Just c
    | s == accCedilla     ++ [0x63] = Just 'ç'
    | s == accCedilla     ++ [0x27] = Just 'Ç'
    | s == accGrave       ++ [0x61] = Just 'à'
    | s == accCirconflexe ++ [0x61] = Just 'â'
    | s == accUmlaut      ++ [0x61] = Just 'ä'
    | s == accGrave       ++ [0x65] = Just 'è'
    | s == accAcute       ++ [0x65] = Just 'é'
    | s == accCirconflexe ++ [0x65] = Just 'ê'
    | s == accUmlaut      ++ [0x65] = Just 'ë'
    | s == accCirconflexe ++ [0x69] = Just 'î'
    | s == accUmlaut      ++ [0x69] = Just 'ï'
    | s == accCirconflexe ++ [0x6F] = Just 'ô'
    | s == accUmlaut      ++ [0x6F] = Just 'ö'
    | s == accGrave       ++ [0x75] = Just 'ù'
    | s == accCirconflexe ++ [0x75] = Just 'û'
    | s == accUmlaut      ++ [0x75] = Just 'ü'
    | otherwise                     = Nothing
    where c = (chr . fromIntegral . head) s

-- | Converts an MString to a key
toKey :: MString -> Key
toKey s
    | isJust alpha           = KeyAlpha (fromJust alpha)
    | s == keyUp             = KeyUp Plain
    | s == keyDown           = KeyDown Plain
    | s == keyLeft           = KeyLeft Plain
    | s == keyRight          = KeyRight Plain
    | s == keyShiftUp        = KeyUp Shift
    | s == keyShiftDown      = KeyDown Shift
    | s == keyShiftLeft      = KeyLeft Shift
    | s == keyShiftRight     = KeyRight Shift
    | s == ctrlLeft          = KeyLeft Ctrl
    | s == keyReturn         = KeyReturn Plain
    | s == keyShiftReturn    = KeyReturn Shift
    | s == keyCtrlReturn     = KeyReturn Ctrl
    | s == keyFuncSend       = KeySend Plain
    | s == keyFuncPrev       = KeyPrev Plain
    | s == keyFuncRepeat     = KeyRepeat Plain
    | s == keyFuncGuide      = KeyGuide Plain
    | s == keyFuncCancel     = KeyCancel Plain
    | s == keyFuncTOC        = KeyTOC Plain
    | s == keyFuncCorrection = KeyCorrection Plain
    | s == keyFuncNext       = KeyNext Plain
    | s == keyFuncConnection = KeyConnection Plain
    | s == accCedilla        = KeyCedilla Plain
    | s == accGrave          = KeyGrave Plain
    | s == accAcute          = KeyAcute Plain
    | s == accCirconflexe    = KeyCircumflex Plain
    | s == accUmlaut         = KeyDiaeresis Plain
    | otherwise              = error $ "Unsupported key: " ++ show s
    where alpha = toAlpha s

