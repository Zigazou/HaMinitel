-- Module Sequence
module Minitel.MString where

import Data.Char
import qualified Data.ByteString as B
import Minitel.Constants

type MString       = [Int]
type MConfirmation = (MString, MString)
type MCall         = (MString, Int)

showInt :: Int -> MString
showInt i = map ord $ show i

toString :: MString -> String
toString = map chr

toByteString :: MString -> B.ByteString
toByteString = B.pack . map fromIntegral

toMString :: B.ByteString -> MString
toMString = map fromIntegral . B.unpack

completeReturn :: MString -> Bool
completeReturn []                 = False
completeReturn [0x19]             = False
completeReturn [0x13]             = False
completeReturn [0x1b]             = False
completeReturn [0x1b, 0x5b]       = False
completeReturn [0x1b, 0x5b, 0x32] = False
completeReturn [0x1b, 0x5b, 0x34] = False
completeReturn _                  = True

-- Special characters conversion tables
toVideotex :: Char -> MString
toVideotex c 
    | c == '£'  = [0x19, 0x23]
    | c == '°'  = [0x19, 0x30]
    | c == '±'  = [0x19, 0x31] 
    | c == '←'  = [0x19, 0x2C]
    | c == '↑'  = [0x19, 0x2D]
    | c == '→'  = [0x19, 0x2E]
    | c == '↓'  = [0x19, 0x2F] 
    | c == '¼'  = [0x19, 0x3C]
    | c == '½'  = [0x19, 0x3D]
    | c == '¾'  = [0x19, 0x3E] 
    | c == 'ç'  = [0x19, 0x4B, 0x63]
    | c == '’'  = [0x19, 0x4B, 0x27] 
    | c == 'à'  = [0x19, 0x41, 0x61]
    | c == 'á'  = [0x19, 0x42, 0x61]
    | c == 'â'  = [0x19, 0x43, 0x61]
    | c == 'ä'  = [0x19, 0x48, 0x61] 
    | c == 'è'  = [0x19, 0x41, 0x65]
    | c == 'é'  = [0x19, 0x42, 0x65]
    | c == 'ê'  = [0x19, 0x43, 0x65]
    | c == 'ë'  = [0x19, 0x48, 0x65] 
    | c == 'ì'  = [0x19, 0x41, 0x69]
    | c == 'í'  = [0x19, 0x42, 0x69]
    | c == 'î'  = [0x19, 0x43, 0x69]
    | c == 'ï'  = [0x19, 0x48, 0x69] 
    | c == 'ò'  = [0x19, 0x41, 0x6F]
    | c == 'ó'  = [0x19, 0x42, 0x6F]
    | c == 'ô'  = [0x19, 0x43, 0x6F]
    | c == 'ö'  = [0x19, 0x48, 0x6F] 
    | c == 'ù'  = [0x19, 0x41, 0x75]
    | c == 'ú'  = [0x19, 0x42, 0x75]
    | c == 'û'  = [0x19, 0x43, 0x75]
    | c == 'ü'  = [0x19, 0x48, 0x75] 
    | c == 'Œ'  = [0x19, 0x6A]
    | c == 'œ'  = [0x19, 0x7A] 
    | c == 'ß'  = [0x19, 0x7B]
    | c == 'β'  = [0x19, 0x7B]
    | isAscii c = [(fromIntegral . ord) c]
    | otherwise = []

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
