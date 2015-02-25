{-|
Module      : HelloWorld
Description : HaMinitel HelloWorld
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

HaMinitel HelloWorld.
-}
module Main where

-- Detailed imports to help you know the roles of each part. 
import Minitel.Minitel ( minitel, (<<<), standardSettings, waitForMinitel )
import Minitel.Generate.Generator ( mString )
import Minitel.Type.Videotex ( MMode (VideoTex) )

main :: IO ()
main = do
    -- Connect to the Minitel with standard settings. /dev/ttyUSB0 is the
    -- standard port when using USB UART based on FT232 chipset.
    m <- minitel "/dev/ttyUSB0" standardSettings

    -- Display Hello, World! on the Minitel screen
    m <<< mString VideoTex "Hello, World!"

    -- Wait till the Minitel has received everything.
    -- It is needed because the serial data transmitted may be cached at various
    -- levels (HaMinitel, OS, modem...)
    waitForMinitel m

