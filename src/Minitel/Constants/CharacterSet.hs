{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-|
Module      : CharacterSet
Description : Character sets
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Character sets.
-}
module Minitel.Constants.CharacterSet where

import qualified Minitel.Constants.C0    as C0

import Minitel.Type.MNatural (MNat)

default (MNat)

-- * Designation of Graphic Sets
sG0G0  = [C0.ESC, 0x28, 0x40]
sG0G1  = [C0.ESC, 0x29, 0x40]
sG0G2  = [C0.ESC, 0x2a, 0x40]
sG0G3  = [C0.ESC, 0x2b, 0x40]
sG'0G0 = [C0.ESC, 0x28, 0x20, 0x42]

sG1G0  = [C0.ESC, 0x28, 0x63]
sG1G1  = [C0.ESC, 0x29, 0x63]
sG1G2  = [C0.ESC, 0x2a, 0x63]
sG1G3  = [C0.ESC, 0x2b, 0x63]
sG'1G1 = [C0.ESC, 0x29, 0x20, 0x43]

sG2G0  = [C0.ESC, 0x28, 0x62]
sG2G1  = [C0.ESC, 0x29, 0x62]
sG2G2  = [C0.ESC, 0x2a, 0x62]
sG2G3  = [C0.ESC, 0x2b, 0x62]

sG3G0  = [C0.ESC, 0x28, 0x64]
sG3G1  = [C0.ESC, 0x29, 0x64]
sG3G2  = [C0.ESC, 0x2a, 0x64]
sG3G3  = [C0.ESC, 0x2b, 0x64]

sGGG0  = [C0.ESC, 0x28, 0x21, 0x40]
sGGG1  = [C0.ESC, 0x29, 0x21, 0x40]
sGGG2  = [C0.ESC, 0x2a, 0x21, 0x40]
sGGG3  = [C0.ESC, 0x2b, 0x21, 0x40]

