{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : C0
Description : C0 set
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Primary control function set (default CO set) - ETS 300 072.
-}
module Minitel.Constants.C0 where

import Minitel.Type.MNatural (MNat)

default (MNat)

pattern NUL = 0x00 -- NULl
pattern APB = 0x08 -- Active Position Back
pattern APF = 0x09 -- Active Position Forward
pattern APD = 0x0a -- Active Position Down
pattern APU = 0x0b -- Active Position Up
pattern CS  = 0x0c -- Clear Screen
pattern APR = 0x0d -- Active Position Return
pattern SO  = 0x0e -- Shift Out
pattern SI  = 0x0f -- Shift In
pattern CON = 0x11 -- Cursor on
pattern RPT = 0x12 -- RePeaT
pattern COF = 0x14 -- Cursor off
pattern CAN = 0x18 -- CANcel
pattern SS2 = 0x19 -- Single Shift 2
pattern ESC = 0x1b -- ESCape
pattern SS3 = 0x1d -- Single Shift 3
pattern APH = 0x1e -- Active Position Home
pattern APA = 0x1f -- Activate Position Address

