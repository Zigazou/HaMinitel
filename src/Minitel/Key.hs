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

-- | Every possible character of the Minitel
minitelChars :: [Char]
minitelChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\
               \äëïöüâêîôûàèùéçœŒ{}*$!:;,?./\\&(-_)=+'@0123456789 \
               \↑←→↓°÷±§"

isMinitelChar :: Char -> Bool
isMinitelChar c = elem c minitelChars

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

-- | Converts an MString to a key
toKey :: MString -> Key
toKey s
    | isMinitelChar c        = KeyAlpha c
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
    | otherwise              = error $ "Unsupported key: " ++ show s
    where c = (chr . fromIntegral . head) s

