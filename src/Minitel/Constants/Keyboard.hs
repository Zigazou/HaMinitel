{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Keyboard
Description : Keyboard values sent by the Minitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Keyboard values sent by the Minitel.
-}
module Minitel.Constants.Keyboard where

import qualified Minitel.Constants.ASCII as ASCII
import Minitel.Constants.CSI

import Minitel.Type.MNatural (MNat)

default (MNat)

-- * Direction keys
pattern Up          = [0x0b]
pattern Down        = [0x0a]
pattern Left        = [0x08]
pattern Right       = [0x09]

pattern ShiftUp    = CSI1 0x4d
pattern ShiftDown  = CSI1 0x4c
pattern ShiftLeft  = CSI1 0x50
pattern ShiftRight = CSI2 0x34 0x68

pattern CtrlLeft = [0x7f]

-- * Return Key
pattern Return      = [0x0d]
pattern ShiftReturn = CSI1 0x48
pattern CtrlReturn  = CSI2 0x32 0x4a

-- * Function Keys
pattern FuncSend       = [ASCII.DC3, 0x41]
pattern FuncPrev       = [ASCII.DC3, 0x42]
pattern FuncRepeat     = [ASCII.DC3, 0x43]
pattern FuncGuide      = [ASCII.DC3, 0x44]
pattern FuncCancel     = [ASCII.DC3, 0x45]
pattern FuncTOC        = [ASCII.DC3, 0x46]
pattern FuncCorrection = [ASCII.DC3, 0x47]
pattern FuncNext       = [ASCII.DC3, 0x48]
pattern FuncConnection = [ASCII.DC3, 0x49]

