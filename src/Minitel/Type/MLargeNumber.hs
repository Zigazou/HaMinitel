{-|
Module      : MLargeNumber
Description : Handles large number for the Minitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

MLargeNumber handles coding of large numbers for the Minitel.
-}
module Minitel.Type.MLargeNumber where

import           Minitel.Type.MNatural
import           Minitel.Type.MString

-- | showInt will return an MString generated from an Int. For example, if it
--   is called showLargeNumber 27, it will return [0x32, 0x37]
showLargeNumber :: Int -> MString
showLargeNumber i
    | i < 0        = error "Negative number !"
    | i == 0       = [mnat 0x40]
    | otherwise    = reverse (0x40 + head b5 : map (+ 0x60) (tail b5))
    where b5 = split5 i

split5 :: Int -> MString
split5 0 = []
split5 i = mnat lo:split5 hi
    where (hi, lo) = divMod i 32

