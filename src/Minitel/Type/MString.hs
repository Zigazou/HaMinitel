{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : MString
Description : Sequence of values for the Minitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

MString Provides a structure holding a sequence of values for the Minitel.
-}
module Minitel.Type.MString
( MString
, MConfirmation
, MCall
, MMString
, (+++)
, showInt
, completeReturn
, toVideotex
, toTerminal
)
where

import Data.Char (ord, isAscii)
import Minitel.Type.MNatural (MNat, mnat)

import qualified Minitel.Constants.ASCII as ASCII
import qualified Minitel.Constants.C0    as C0
import qualified Minitel.Constants.SSCFS as SSCFS
import Minitel.Constants.Accents
import Minitel.Constants.MUnicode

import Control.Applicative ((<|>), (<$>), (<*>))

default (MNat)

-- | An MString is just an Int list
type MString       = [MNat]

-- | An MConfirmation is composed of two MString, the first is to be sent to
--   the Minitel and the second is what we should receive if everything went
--   well
type MConfirmation = (MString, MString)

-- | An MCall is composed ot an MString and an Int, the MString is to be sent
--   to the Minitel and the Int is the length of the MString we should receive
--   if everything went well
type MCall         = (MString, MNat)

-- | An MMString is a Maybe list of MString
type MMString      = Maybe [MString]

-- | Concatenates MMString
(+++) :: Maybe [a] -> Maybe [a] -> Maybe [a]
(+++) x y = (++) <$> x <*> y <|> x <|> y

-- | showInt will return an MString generated from an Int. For example, if it
--   is called showInt 27, it will return [0x32, 0x37]
showInt :: MNat -> MString
showInt i = map (mnat . ord) $ show i

-- | Check if an MString received from the Minitel is complete. For example,
--   if the Minitel sent us an 0x19, it should be followed by another value.
completeReturn :: MString -> Bool
completeReturn []                        = False
completeReturn [C0.SS2]                  = False
completeReturn [ASCII.DC3]               = False
completeReturn [C0.ESC]                  = False
completeReturn [C0.ESC, SSCFS.CSI]       = False
completeReturn [C0.ESC, SSCFS.CSI, 0x32] = False
completeReturn [C0.ESC, SSCFS.CSI, 0x34] = False
completeReturn Cedilla                   = False
completeReturn Grave                     = False
completeReturn Acute                     = False
completeReturn Circumflex                = False
completeReturn Umlaut                    = False
completeReturn _                         = True

-- | Translates a unicode character to its VideoTex counterpart. Standard ASCII
--   characters are kept as the Minitel is based upon it. If the character has
--   no VideoTex counterpart, it is simply ignored and suppressed from the
--   outputted MString
toVideotex :: Char -> MString
toVideotex c
    | c == poundSign                       = [C0.SS2, 0x23]
    | c == degreeSign                      = [C0.SS2, 0x30]
    | c == plusMinusSign                   = [C0.SS2, 0x31]
    | c == leftwardsArrow                  = [C0.SS2, 0x2C]
    | c == upwardsArrow                    = [C0.SS2, 0x2D]
    | c == rightwardsArrow                 = [C0.SS2, 0x2E]
    | c == downwardsArrow                  = [C0.SS2, 0x2F]
    | c == vulgarFractionOneQuarter        = [C0.SS2, 0x3C]
    | c == vulgarFractionOneHalf           = [C0.SS2, 0x3D]
    | c == vulgarFractionThreeQuarters     = [C0.SS2, 0x3E]
    | c == latinSmallLetterCWithCedilla    = AddCedilla    ASCII.LowerC
    | c == rightSingleQuotationMark        = AddCedilla    ASCII.Quote
    | c == latinSmallLetterAWithGrave      = AddGrave      ASCII.LowerA
    | c == latinSmallLetterAWithAcute      = AddAcute      ASCII.LowerA
    | c == latinSmallLetterAWithCircumflex = AddCircumflex ASCII.LowerA
    | c == latinSmallLetterAWithDiaeresis  = AddUmlaut     ASCII.LowerA
    | c == latinSmallLetterEWithGrave      = AddGrave      ASCII.LowerE
    | c == latinSmallLetterEWithAcute      = AddAcute      ASCII.LowerE
    | c == latinSmallLetterEWithCircumflex = AddCircumflex ASCII.LowerE
    | c == latinSmallLetterEWithDiaeresis  = AddUmlaut     ASCII.LowerE
    | c == latinSmallLetterIWithGrave      = AddGrave      ASCII.LowerI
    | c == latinSmallLetterIWithAcute      = AddAcute      ASCII.LowerI
    | c == latinSmallLetterIWithCircumflex = AddCircumflex ASCII.LowerI
    | c == latinSmallLetterIWithDiaeresis  = AddUmlaut     ASCII.LowerI
    | c == latinSmallLetterOWithGrave      = AddGrave      ASCII.LowerO
    | c == latinSmallLetterOWithAcute      = AddAcute      ASCII.LowerO
    | c == latinSmallLetterOWithCircumflex = AddCircumflex ASCII.LowerO
    | c == latinSmallLetterOWithDiaeresis  = AddUmlaut     ASCII.LowerO
    | c == latinSmallLetterUWithGrave      = AddGrave      ASCII.LowerU
    | c == latinSmallLetterUWithAcute      = AddAcute      ASCII.LowerU
    | c == latinSmallLetterUWithCircumflex = AddCircumflex ASCII.LowerU
    | c == latinSmallLetterUWithDiaeresis  = AddUmlaut     ASCII.LowerU
    | c == latinCapitalLigatureOE          = [C0.SS2, 0x6A]
    | c == latinSmallLigatureOE            = [C0.SS2, 0x7A]
    | c == latinSmallLetterSharpS          = [C0.SS2, 0x7B]
    | c == greekSmallLetterBeta            = [C0.SS2, 0x7B]
    | isAscii c                            = [(fromIntegral . ord) c]
    | otherwise                            = []

-- | Translates a unicode character to its TeleInformatique (terminal mode)
--   counterpart. Standard ASCII characters are kept as the Minitel is based
--   upon it. If the character has no TeleInformatique counterpart, it is
--   simply ignored and suppressed from the outputted MString
toTerminal :: Char -> MString
toTerminal c
    | c == poundSign                       = [C0.SO, 0x23, C0.SI]
    | c == degreeSign                      = [C0.SO, 0x5B, C0.SI]
    | c == latinSmallLetterCWithCedilla    = [C0.SO, 0x5C, C0.SI]
    | c == rightSingleQuotationMark        = [ASCII.Quote]
    | c == graveAccent                     = [ASCII.BackTick]
    | c == sectionSign                     = [C0.SO, 0x5D, C0.SI]
    | c == latinSmallLetterAWithGrave      = [C0.SO, 0x40, C0.SI]
    | c == latinSmallLetterEWithGrave      = [C0.SO, 0x7F, C0.SI]
    | c == latinSmallLetterEWithAcute      = [C0.SO, 0x7B, C0.SI]
    | c == latinSmallLetterUWithGrave      = [C0.SO, 0x7C, C0.SI]
    | isAscii c                            = [(fromIntegral . ord) c]
    | otherwise                            = []

