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
module Minitel.MString where

import Data.Char
import qualified Data.ByteString as B
import Minitel.Constants

-- | An MString is just an Int list
type MString       = [Int]

-- | An MConfirmation is composed of two MString, the first is to be sent to
--   the Minitel and the second is what we should receive if everything went
--   well
type MConfirmation = (MString, MString)

-- | An MCall is composed ot an MString and an Int, the MString is to be sent
--   to the Minitel and the Int is the length of the MString we should receive
--   if everything went well
type MCall         = (MString, Int)

-- | showInt will return an MString generated from an Int. For example, if it
--   is called showInt 27, it will return [0x32, 0x37]
showInt :: Int -> MString
showInt i = map ord $ show i

-- | Check if an MString received from the Minitel is complete. For example,
--   if the Minitel sent us an 0x19, it should be followed by another value.
completeReturn :: MString -> Bool
completeReturn []                 = False
completeReturn [0x19]             = False
completeReturn [0x13]             = False
completeReturn [0x1b]             = False
completeReturn [0x1b, 0x5b]       = False
completeReturn [0x1b, 0x5b, 0x32] = False
completeReturn [0x1b, 0x5b, 0x34] = False
completeReturn _                  = True

-- | Translates a unicode character to its VideoTex counterpart. Standard ASCII
--   characters are kept as the Minitel is based upon it. If the character has
--   no VideoTex counterpart, it is simply ignored and suppressed from the
--   outputted MString
toVideotex :: Char -> MString
toVideotex c 
    | c == '£'  = [ss2, 0x23]
    | c == '°'  = [ss2, 0x30]
    | c == '±'  = [ss2, 0x31] 
    | c == '←'  = [ss2, 0x2C]
    | c == '↑'  = [ss2, 0x2D]
    | c == '→'  = [ss2, 0x2E]
    | c == '↓'  = [ss2, 0x2F] 
    | c == '¼'  = [ss2, 0x3C]
    | c == '½'  = [ss2, 0x3D]
    | c == '¾'  = [ss2, 0x3E] 
    | c == 'ç'  = accCedilla     ++ [0x63]
    | c == '’'  = accCedilla     ++ [0x27] 
    | c == 'à'  = accGrave       ++ [0x61]
    | c == 'á'  = accAcute       ++ [0x61]
    | c == 'â'  = accCirconflexe ++ [0x61]
    | c == 'ä'  = accUmlaut      ++ [0x61] 
    | c == 'è'  = accGrave       ++ [0x65]
    | c == 'é'  = accAcute       ++ [0x65]
    | c == 'ê'  = accCirconflexe ++ [0x65]
    | c == 'ë'  = accUmlaut      ++ [0x65] 
    | c == 'ì'  = accGrave       ++ [0x69]
    | c == 'í'  = accAcute       ++ [0x69]
    | c == 'î'  = accCirconflexe ++ [0x69]
    | c == 'ï'  = accUmlaut      ++ [0x69] 
    | c == 'ò'  = accGrave       ++ [0x6F]
    | c == 'ó'  = accAcute       ++ [0x6F]
    | c == 'ô'  = accCirconflexe ++ [0x6F]
    | c == 'ö'  = accUmlaut      ++ [0x6F] 
    | c == 'ù'  = accGrave       ++ [0x75]
    | c == 'ú'  = accAcute       ++ [0x75]
    | c == 'û'  = accCirconflexe ++ [0x75]
    | c == 'ü'  = accUmlaut      ++ [0x75] 
    | c == 'Œ'  = [ss2, 0x6A]
    | c == 'œ'  = [ss2, 0x7A] 
    | c == 'ß'  = [ss2, 0x7B]
    | c == 'β'  = [ss2, 0x7B]
    | isAscii c = [(fromIntegral . ord) c]
    | otherwise = []

-- | Translates a unicode character to its TeleInformatique (terminal mode)
--   counterpart. Standard ASCII characters are kept as the Minitel is based
--   upon it. If the character has no TeleInformatique counterpart, it is
--   simply ignored and suppressed from the outputted MString
toTerminal :: Char -> MString
toTerminal c
    | c == '£'  = [0x0E, 0x23, 0x0F]
    | c == '°'  = [0x0E, 0x5B, 0x0F]
    | c == 'ç'  = [0x0E, 0x5C, 0x0F]
    | c == '’'  = [0x27]
    | c == '`'  = [0x60]
    | c == '§'  = [0x0E, 0x5D, 0x0F]
    | c == 'à'  = [0x0E, 0x40, 0x0F]
    | c == 'è'  = [0x0E, 0x7F, 0x0F]
    | c == 'é'  = [0x0E, 0x7B, 0x0F]
    | c == 'ù'  = [0x0E, 0x7C, 0x0F]
    | isAscii c = [(fromIntegral . ord) c]
    | otherwise = []
