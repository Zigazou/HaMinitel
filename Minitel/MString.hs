-- Module Sequence
module Minitel.MString where

import Data.Char
import qualified Data.ByteString as B
import Minitel.Constants

type MString       = [Integer]
type MConfirmation = (MString, MString)
type MCall         = (MString, Integer)

showInteger :: Integer -> MString
showInteger i = map (fromIntegral . ord) $ show i

toString :: MString -> String
toString = map (chr . fromIntegral)

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

