{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : PSCFS
Description : Parallel supplementary control function set
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Parallel supplementary control function set - ETS 300 072.
-}
module Minitel.Constants.PSCFS where

import Minitel.Type.MNatural (MNat)
import qualified Minitel.Constants.C0 as C0

default (MNat)

pattern PSCFS = [C0.ESC, 0x22, 0x41]

pattern BKF = 0x40 -- BlacK Foreground
pattern RDF = 0x41 -- ReD Foreground
pattern GRF = 0x42 -- GReen Foreground
pattern YLF = 0x43 -- YeLlow Foreground
pattern BLF = 0x44 -- BLue Foreground
pattern MGF = 0x45 -- MaGenta Foreground
pattern CNF = 0x46 -- CyaN Foreground
pattern WHF = 0x47 -- WHite Foreground
pattern BKB = 0x50 -- BlacK Background
pattern RDB = 0x51 -- ReD Background
pattern GRB = 0x52 -- GReen Background
pattern YLB = 0x53 -- YeLlow Background
pattern BLB = 0x54 -- BLue Background
pattern MGB = 0x55 -- MaGenta Background
pattern CNB = 0x56 -- CyaN Background
pattern WHB = 0x57 -- WHite Background
pattern NPO = 0x5c -- Normal POlarity
pattern IPO = 0x5d -- Inverted POlarity
pattern TRB = 0x5e -- 
pattern STC = 0x5f -- STop Conceal

