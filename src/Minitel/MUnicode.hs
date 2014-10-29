{-|
Module      : MUnicode
Description : Unicode values understandable by the Minitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Unicode values understandable by the Minitel
-}
module Minitel.MUnicode where

import           Data.Char

poundSign                       = chr 0xa3
degreeSign                      = chr 0xb0
plusMinusSign                   = chr 0xb1

leftwardsArrow                  = chr 0x2190
upwardsArrow                    = chr 0x2191
rightwardsArrow                 = chr 0x2192
downwardsArrow                  = chr 0x2193

vulgarFractionOneQuarter        = chr 0xbc
vulgarFractionOneHalf           = chr 0xbd
vulgarFractionThreeQuarters     = chr 0xbe
rightSingleQuotationMark        = chr 0x2019

latinSmallLetterCWithCedilla    = chr 0xe7

latinSmallLetterAWithGrave      = chr 0xe0
latinSmallLetterAWithAcute      = chr 0xe1
latinSmallLetterAWithCircumflex = chr 0xe2
latinSmallLetterAWithDiaeresis  = chr 0xe4

latinSmallLetterEWithGrave      = chr 0xe8
latinSmallLetterEWithAcute      = chr 0xe9
latinSmallLetterEWithCircumflex = chr 0xea
latinSmallLetterEWithDiaeresis  = chr 0xeb

latinSmallLetterIWithGrave      = chr 0xec
latinSmallLetterIWithAcute      = chr 0xed
latinSmallLetterIWithCircumflex = chr 0xee
latinSmallLetterIWithDiaeresis  = chr 0xef

latinSmallLetterOWithGrave      = chr 0xf2
latinSmallLetterOWithAcute      = chr 0xf3
latinSmallLetterOWithCircumflex = chr 0xf4
latinSmallLetterOWithDiaeresis  = chr 0xf6

latinSmallLetterUWithGrave      = chr 0xf9
latinSmallLetterUWithAcute      = chr 0xfa
latinSmallLetterUWithCircumflex = chr 0xfb
latinSmallLetterUWithDiaeresis  = chr 0xfc

latinCapitalLigatureOE          = chr 0x0152
latinSmallLigatureOE            = chr 0x0153

latinSmallLetterSharpS          = chr 0xdf
greekSmallLetterBeta            = chr 0x03b2

graveAccent                     = chr 0x60
sectionSign                     = chr 0xa7
