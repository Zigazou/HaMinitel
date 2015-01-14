{-|
Module      : To87
Description : Converts 8 bits to 7 bits
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module converts 8 bits bytes to 7 bits bytes.
-}
module Minitel.Generate.Photo.To87 ( to87 ) where

import Data.Word
import Data.Bits
import Data.Binary.Bits.Get
import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy.Char8 as BS

convert87 :: Int -> BitGet [Word8]
convert87 n
    | n < 8     = do bits <- getWord8 n
                     return [shiftL bits (7 - n)]
    | otherwise = do bits <- getWord8 7
                     rest <- convert87 (n-7)
                     return $ bits : rest

to87 :: BS.ByteString -> [Word8]
to87 datas = runGet (runBitGet (convert87 len)) datas
           where len = fromIntegral $ BS.length datas * 8

