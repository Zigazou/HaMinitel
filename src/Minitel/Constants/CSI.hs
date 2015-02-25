{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : CSI
Description : Control Sequence Introduccer
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Control Sequence Introducer.
-}
module Minitel.Constants.CSI where

import Minitel.Type.MNatural (MNat)
import qualified Minitel.Constants.ASCII as ASCII
import qualified Minitel.Constants.C0    as C0
import qualified Minitel.Constants.SSCFS as SSCFS
import Minitel.Type.MString (MString, showInt)
import Data.List (intercalate)

pattern CSI1 x     = [C0.ESC, SSCFS.CSI, x   ]
pattern CSI2 x y   = [C0.ESC, SSCFS.CSI, x, y]
pattern CSIx xs    = C0.ESC:SSCFS.CSI:xs

sCSI :: MString
sCSI = [C0.ESC, SSCFS.CSI] -- Control Sequence Introducer

-- | Generate a CSI sequence
mCSI :: MNat -> [MNat] -> MString
mCSI cmd parms =
    CSIx $ intercalate [ASCII.SemiColon] (fmap showInt parms) ++ [cmd]
