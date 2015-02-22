{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : SSCFS
Description : Serial supplementary control function set
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Serial supplementary control function set - ETS 300 072.
-}
module Minitel.Constants.SSCFS where

import Minitel.Type.MNatural (MNat)
import qualified Minitel.Constants.C0 as C0

default (MNat)

pattern SSCFS = [C0.ESC, 0x22, 0x40]

pattern ABK = 0x40 -- Alpha BlacK
pattern ANR = 0x41 -- Alpha Red
pattern ANG = 0x42 -- Alpha Green
pattern ANY = 0x43 -- Alpha Yellow
pattern ANB = 0x44 -- Alpha Blue
pattern ANM = 0x45 -- Alpha Magenta
pattern ANC = 0x46 -- Alpha Cyan
pattern ANW = 0x47 -- Alpha White
pattern FSH = 0x48 -- FlaSH
pattern STD = 0x49 -- STeaDy
pattern EBX = 0x4a -- End BoX
pattern SBX = 0x4b -- Start BoX
pattern NSZ = 0x4c -- Normal SiZe
pattern DBH = 0x4d -- DouBle Height
pattern DBW = 0x4e -- DouBle Width
pattern DBS = 0x4f -- DouBle Size
pattern MBK = 0x50 -- Mosaic BlacK
pattern MSR = 0x51 -- MoSaic Red
pattern MSG = 0x52 -- MoSaic Green
pattern MSY = 0x53 -- MoSaic Yellow
pattern MSB = 0x54 -- MoSaic Blue
pattern MSM = 0x55 -- MoSaic Magenta
pattern MSC = 0x56 -- MoSaic Cyan
pattern MSW = 0x57 -- MoSaic White
pattern CDY = 0x58 -- Conceal DisplaY
pattern SPL = 0x59 -- StoP Lining
pattern STL = 0x5a -- StarT Lining
pattern CSI = 0x5b -- Control Sequence Introducer
pattern BBD = 0x5c -- Black BackgrounD
pattern NBD = 0x5d -- New BackgrounD
pattern HMS = 0x5e -- 
pattern RMS = 0x5f -- 

