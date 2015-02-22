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
import qualified Minitel.Constants.ASCII as ASCII
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

-- | Show user friendly version of an MNat value. The output is padded on 8
--   character wide, making it suitable to display sequentially on the Minitel
--   screen (40 characters wide) or the computer screen (generally 80 characters
--   wide).
friendly :: MNat -> String
friendly mn
    | mn < ASCII.Space = printf " %s-%02x "   (controls !! mni) mni
    | otherwise        = printf " [%c]-%02x " mni mni
    where mni = fromMNat mn

-- | The endless Minitel listener
listen :: Minitel -> IO ()
listen m = do
    -- Read exactly one byte from the Minitel (blocking read)
    byte <- readBCount (getter m) 1

    -- Get a human friendly view of the value
    let human = (friendly . head) byte

    -- Display it on the computer screen
    putStr human
    hFlush stdout

    -- Display it on the Minitel
    m <<< mString VideoTex human

    -- Do the same for the following bytes
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

    -- How this program works
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
    where crlf = [ ASCII.CR, ASCII.LF ]

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

