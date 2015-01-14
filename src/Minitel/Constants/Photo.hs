{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-|
Module      : Photo
Description : Constants for Photo Videotex
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

-}
module Minitel.Constants.Photo where

import           Minitel.Type.MNatural

-- No need for Integer default values, they are overkill for the Minitel
default (MNat)

-- * Attribute codes
psa = 0x20                          -- Parameter Status Attribute
pda = 0x21                          -- Picture Display Attributes
spa = 0x22                          -- Source Picture Attributes
ssa = 0x23                          -- Source Signal Attributes
sca = 0x24                          -- Source Coding Algorithm Attributes
tca = 0x25                          -- Transmission Channel Attribute

-- * Parameter status attributes
rtd = [psa, 0x30]                   -- Reset To Default

-- * Picture Display Attributes
fsd = [pda, 0x30]                   -- Full Screen Display
asr = [pda, 0x31]                   -- Source Aspect Ratio
loc = [pda, 0x32]                   -- Photo-area LOCation
pas = [pda, 0x33]                   -- Photo-Area Size
ppl = [pda, 0x34]                   -- Picture PLacement
cpa = [pda, 0x35]                   -- Clear Photo-Area

-- * Source Picture Attributes
pct = [spa, 0x30]                   -- Source Picture Comments
pds = [spa, 0x31]                   -- Source Picture Dimensions
pid = [spa, 0x32]                   -- Source Pixel Density
swd = [spa, 0x33]                   -- Source Pixel Sweep Direction
dci = [spa, 0x34]                   -- DC Images

-- * Source Signal Attributes
scd = [ssa, 0x30]                   -- Source Component Description
cdp = [ssa, 0x31]                   -- Source Component Data Precision
cmo = [ssa, 0x32]                   -- Source Component Order
las = [ssa, 0x33]                   -- Source Level Assignment

-- * Source Coding Algorithm Attributes
jpg = [sca, 0x30]                   -- JPEG Coding Mode
etm = [sca, 0x31]                   -- Encoding Table Management
ama = [sca, 0x32]                   -- Application Marker Codes Assignment

-- * Transmission Channel Attributes
tme = [tca, 0x30]                   -- Translation Mode Encoding

-- * Sub-parameter codes
pInteger     = 0x40
pReal        = 0x41
pNormalised  = 0x42
pDecimal     = 0x43
pEnumeration = 0x44
pBoolean     = 0x45
pString      = 0x46

