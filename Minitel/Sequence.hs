-- Module Sequence
module Minitel.Sequence where

import Data.Char
import qualified Data.ByteString as B
import Minitel.Constantes

type SeqMinitel = [Integer]
type SeqValide = (SeqMinitel, SeqMinitel)
type SeqAppel = (SeqMinitel, Integer)

seqIntString :: Integer -> SeqMinitel
seqIntString i = map (fromIntegral . ord) $ show i

stringSeq :: SeqMinitel -> String
stringSeq = map (chr . fromIntegral)

seqByteString :: SeqMinitel -> B.ByteString
seqByteString = B.pack . map fromIntegral

seqMinitel :: B.ByteString -> SeqMinitel
seqMinitel = map fromIntegral . B.unpack
