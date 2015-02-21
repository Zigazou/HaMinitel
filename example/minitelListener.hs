{-|
Module      : MinitelListener
Description : Listen to the Minitel
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This program listens to the Minitel and shows the codes it sends to us.
-}
module Main where

-- Detailed imports to help you know the roles of each part. 
import Minitel.Type.MNatural (MNat, fromMNat)
import Minitel.Minitel
       (Minitel, readBCount, getter, minitel, (<<<), baseSettings)
import Minitel.Generate.Generator
       ( mClear, mLocate, mString, mSize, mForeground )
import Minitel.Generate.Configuration
       ( mExtendedKeyboard, mCursorKeys, mLowercaseKeyboard, mEchoing
       , mRollMode
       )
import Minitel.Type.MString (MString)
import Minitel.Type.Videotex
       ( WhatToClear (ReallyEverything)
       , MMode (VideoTex)
       , CharWidth (DoubleWidth)
       , CharHeight (DoubleHeight)
       , Color (Green, Yellow, White)
       )
import Minitel.Constants.Constants (aCR, aLF)
import Text.Printf (printf)
import System.IO (hFlush, stdout)

-- | ASCII values below 0x20 are control codes and are not visible. We will
--   display their names instead.
controls :: [String]
controls =
    [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL"
    , "BS ", "HT ", "LF ", "VT ", "FF ", "CR ", "SO ", "SI "
    , "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB"
    , "CAN", "EM ", "SUB", "ESC", "FS ", "GS ", "RS ", "US "
    ]

-- | Show user friendly version of an MNat value.
friendly :: MNat -> String
friendly mn
    | mn < 0x20 = printf " %s-%02x "   (controls !! mni) mni
    | otherwise = printf " [%c]-%02x " mni mni
    where mni = fromMNat mn

-- | The endless Minitel listener
listen :: Minitel -> IO ()
listen m = do
    byte <- readBCount (getter m) 1
    let human = (friendly . head) byte
    putStr human
    hFlush stdout
    m <<< mString VideoTex human
    listen m

-- | Explanations about this program
message :: [MString]
message =
    [ mClear      ReallyEverything

    -- Writes on the status line
    , mLocate     1 0
    , mString     VideoTex "HaMinitel - Minitel listener"

    -- Title
    , mLocate     1 2
    , mSize       DoubleWidth DoubleHeight
    , mForeground Green
    , mString     VideoTex "Minitel listener"

    -- Form background
    , mLocate     1 3
    , mForeground Yellow
    , mString     VideoTex "Type any key to see which codes the", crlf
    , mString     VideoTex "Minitel sends to the computer.", crlf, crlf
    , mString     VideoTex "Information is sent to both the computer", crlf
    , mString     VideoTex "and the Minitel.", crlf, crlf
    , mString     VideoTex "To stop the program, do a CTRL+C on the", crlf
    , mString     VideoTex "computer side.", crlf, crlf
    
    , mForeground White
    ]
    where crlf = [ aCR, aLF ]

main :: IO ()
main = do
    -- Connect to the Minitel with standard settings. /dev/ttyUSB0 is the
    -- standard port when using USB UART based on FT232 chipset.
    m <- minitel "/dev/ttyUSB0" baseSettings

    -- Configure the Minitel keyboard. By default, the Minitel keyboard outputs
    -- uppercase letters, echoes every key and does not listen to special keys.
    m <<< [ mExtendedKeyboard  True
          , mCursorKeys        True
          , mLowercaseKeyboard True
          , mEchoing           False
          , mRollMode          True
          ]

    -- Display explanations about this program
    m <<< message

    -- Endlessly listen so the Minitel
    listen m

