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
module Minitel.Type.MString where

import           Data.Char
import           Minitel.Constants.Constants
import           Minitel.Type.MNatural
import           Minitel.Constants.MUnicode

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

-- | showInt will return an MString generated from an Int. For example, if it
--   is called showInt 27, it will return [0x32, 0x37]
showInt :: MNat -> MString
showInt i = map (mnat . ord) $ show i

-- | Check if an MString received from the Minitel is complete. For example,
--   if the Minitel sent us an 0x19, it should be followed by another value.
completeReturn :: MString -> Bool
completeReturn []                 = False
completeReturn [0x19]             = False -- eSS2
completeReturn [0x13]             = False -- aDC3
completeReturn [0x1b]             = False -- eESC
completeReturn [0x1b, 0x5b]       = False -- eESC, eCSI
completeReturn [0x1b, 0x5b, 0x32] = False -- eESC, eCSI, 0x32
completeReturn [0x1b, 0x5b, 0x34] = False -- eESC, eCSI, 0x34
completeReturn _                  = True

-- | Translates a unicode character to its VideoTex counterpart. Standard ASCII
--   characters are kept as the Minitel is based upon it. If the character has
--   no VideoTex counterpart, it is simply ignored and suppressed from the
--   outputted MString
toVideotex :: Char -> MString
toVideotex c
    | c == poundSign                       = [eSS2, 0x23]
    | c == degreeSign                      = [eSS2, 0x30]
    | c == plusMinusSign                   = [eSS2, 0x31]
    | c == leftwardsArrow                  = [eSS2, 0x2C]
    | c == upwardsArrow                    = [eSS2, 0x2D]
    | c == rightwardsArrow                 = [eSS2, 0x2E]
    | c == downwardsArrow                  = [eSS2, 0x2F]
    | c == vulgarFractionOneQuarter        = [eSS2, 0x3C]
    | c == vulgarFractionOneHalf           = [eSS2, 0x3D]
    | c == vulgarFractionThreeQuarters     = [eSS2, 0x3E]
    | c == latinSmallLetterCWithCedilla    = accCedilla     ++ [0x63]
    | c == rightSingleQuotationMark        = accCedilla     ++ [0x27]
    | c == latinSmallLetterAWithGrave      = accGrave       ++ [0x61]
    | c == latinSmallLetterAWithAcute      = accAcute       ++ [0x61]
    | c == latinSmallLetterAWithCircumflex = accCirconflexe ++ [0x61]
    | c == latinSmallLetterAWithDiaeresis  = accUmlaut      ++ [0x61]
    | c == latinSmallLetterEWithGrave      = accGrave       ++ [0x65]
    | c == latinSmallLetterEWithAcute      = accAcute       ++ [0x65]
    | c == latinSmallLetterEWithCircumflex = accCirconflexe ++ [0x65]
    | c == latinSmallLetterEWithDiaeresis  = accUmlaut      ++ [0x65]
    | c == latinSmallLetterIWithGrave      = accGrave       ++ [0x69]
    | c == latinSmallLetterIWithAcute      = accAcute       ++ [0x69]
    | c == latinSmallLetterIWithCircumflex = accCirconflexe ++ [0x69]
    | c == latinSmallLetterIWithDiaeresis  = accUmlaut      ++ [0x69]
    | c == latinSmallLetterOWithGrave      = accGrave       ++ [0x6F]
    | c == latinSmallLetterOWithAcute      = accAcute       ++ [0x6F]
    | c == latinSmallLetterOWithCircumflex = accCirconflexe ++ [0x6F]
    | c == latinSmallLetterOWithDiaeresis  = accUmlaut      ++ [0x6F]
    | c == latinSmallLetterUWithGrave      = accGrave       ++ [0x75]
    | c == latinSmallLetterUWithAcute      = accAcute       ++ [0x75]
    | c == latinSmallLetterUWithCircumflex = accCirconflexe ++ [0x75]
    | c == latinSmallLetterUWithDiaeresis  = accUmlaut      ++ [0x75]
    | c == latinCapitalLigatureOE          = [eSS2, 0x6A]
    | c == latinSmallLigatureOE            = [eSS2, 0x7A]
    | c == latinSmallLetterSharpS          = [eSS2, 0x7B]
    | c == greekSmallLetterBeta            = [eSS2, 0x7B]
    | isAscii c                            = [(fromIntegral . ord) c]
    | otherwise                            = []

-- | Translates a unicode character to its TeleInformatique (terminal mode)
--   counterpart. Standard ASCII characters are kept as the Minitel is based
--   upon it. If the character has no TeleInformatique counterpart, it is
--   simply ignored and suppressed from the outputted MString
toTerminal :: Char -> MString
toTerminal c
    | c == poundSign                       = [0x0E, 0x23, 0x0F]
    | c == degreeSign                      = [0x0E, 0x5B, 0x0F]
    | c == latinSmallLetterCWithCedilla    = [0x0E, 0x5C, 0x0F]
    | c == rightSingleQuotationMark        = [0x27]
    | c == graveAccent                     = [0x60]
    | c == sectionSign                     = [0x0E, 0x5D, 0x0F]
    | c == latinSmallLetterAWithGrave      = [0x0E, 0x40, 0x0F]
    | c == latinSmallLetterEWithGrave      = [0x0E, 0x7F, 0x0F]
    | c == latinSmallLetterEWithAcute      = [0x0E, 0x7B, 0x0F]
    | c == latinSmallLetterUWithGrave      = [0x0E, 0x7C, 0x0F]
    | isAscii c                            = [(fromIntegral . ord) c]
    | otherwise                            = []
