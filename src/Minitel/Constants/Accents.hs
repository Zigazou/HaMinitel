{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Accents
Description : Accents
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Accents
-}
module Minitel.Constants.Accents where

import qualified Minitel.Constants.C0 as C0
import Minitel.Type.MNatural (MNat)

default (MNat)

-- * Accents (for the VideoTex mode)
pattern Accent x      = [C0.SS2, x]
pattern AddAccent x y = [C0.SS2, x, y]

pattern Cedilla    = Accent 0x4b
pattern Grave      = Accent 0x41
pattern Acute      = Accent 0x42
pattern Circumflex = Accent 0x43
pattern Umlaut     = Accent 0x48

pattern AddCedilla    x = AddAccent 0x4b x
pattern AddGrave      x = AddAccent 0x41 x
pattern AddAcute      x = AddAccent 0x42 x
pattern AddCircumflex x = AddAccent 0x43 x
pattern AddUmlaut     x = AddAccent 0x48 x

