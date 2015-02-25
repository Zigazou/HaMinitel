{-|
Module      : Generator
DC0.ESCription : MString generator
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module provides functions generating MString for the Minitel. It allows
you to generate special commands for the Minitel like clearing screen, cursor
position etc. without remembering the codes.
-}
module Minitel.Generate.Generator
( mString
, mForeground
, mBackground
, mLocate
, mMove
, mGotoStartOfLine
, mSize
, mUnderscore
, mBlink
, mReverse
, mClear
, mRepeat
, mBeep
, mRemove
, mInsert
, mSemigraphic
, mUseSet
, mRectangle
)
where

import Minitel.Constants.CharacterSet
import Minitel.Constants.CSI
import qualified Minitel.Constants.ASCII as ASCII
import qualified Minitel.Constants.ANSI  as ANSI
import qualified Minitel.Constants.C0    as C0
import qualified Minitel.Constants.SSCFS as SSCFS
import qualified Minitel.Constants.PSCFS as PSCFS
import Minitel.Type.MNatural (MNat, mnat, fromMNat)
import Minitel.Type.MString (MString, toVideotex, toTerminal)
import Minitel.Type.Videotex
       ( MMode (VideoTex)
       , ToMColor
       , toMColor
       , CharWidth (SimpleWidth, DoubleWidth)
       , CharHeight (SimpleHeight, DoubleHeight)
       , WhatToClear ( Everything, EndOfLine, EndOfScreen, StartOfScreen
                     , StartOfLine, Line, StatusLine, EndOfLine
                     , ReallyEverything
                     )
       , WhatToRemove (Column, Row)
       , WhatToInsert
       , CharSet (G0, G1, G'0, G'1)
       )

default (MNat)

-- | Convert a unicode string into an MString according to a mode. VideoTex
--   offers the best conversion.
mString :: MMode -> String -> MString
mString VideoTex s = concatMap toVideotex s
mString _ s = concatMap toTerminal s

-- | Set foreground color
mForeground :: (ToMColor a) => a -> MString
mForeground color = [C0.ESC, 0x40 + toMColor color]

-- | Set background color
mBackground :: (ToMColor a) => a -> MString
mBackground color = [C0.ESC, 0x50 + toMColor color]

-- | Move cursor to an absolute position. Rows and columns starts at 1
--   The 0 row is the status line (at the very top of the screen) and is
--   handled differently from the rest of the screen by the Minitel.
mLocate :: MNat -> MNat -> MString
mLocate 1 1 = [C0.APH]
mLocate x y = [C0.APA, 0x40 + y, 0x40 + x]

-- | Move cursor relatively to its current position. The function optimizes
--   the length of the generated MString
mMove :: Int -> Int -> MString
mMove 0 0 = []
mMove 0 y | y > 127 || y < -127 = error "y move too big"
          | y >= -4 && y <= -1  = replicate (abs y) C0.APU
          | y >= 1  && y <=  4  = replicate y C0.APD
          | y <  0              = mCSI ANSI.CUD [mnat $ abs y]
          | y >  0              = mCSI ANSI.CUU [mnat y]
mMove x 0 | x > 127 || x < -127 = error "x move too big"
          | x >= -4 && x <= -1  = replicate (abs x) C0.APB
          | x >= 1  && x <=  4  = replicate x C0.APF
          | x <  0              = mCSI ANSI.CUF [mnat $ abs x]
          | x >  0              = mCSI ANSI.CUB [mnat x]
mMove x y = mMove x 0 ++ mMove 0 y

-- | Move cursor to the start of the current line.
mGotoStartOfLine :: MString
mGotoStartOfLine = [C0.APR]

-- | Change character size. The first argument is the width, the second the
--   height
mSize :: CharWidth -> CharHeight -> MString
mSize SimpleWidth SimpleHeight = [C0.ESC, SSCFS.NSZ]
mSize SimpleWidth DoubleHeight = [C0.ESC, SSCFS.DBH]
mSize DoubleWidth SimpleHeight = [C0.ESC, SSCFS.DBW]
mSize DoubleWidth DoubleHeight = [C0.ESC, SSCFS.DBS]

-- | Enable or disable characters underscoring. No underscoring by default.
mUnderscore :: Bool -> MString
mUnderscore True  = [C0.ESC, SSCFS.STL]
mUnderscore False = [C0.ESC, SSCFS.SPL]

-- | Enable or disable characters blinking. No blinking by default.
mBlink :: Bool -> MString
mBlink True  = [C0.ESC, SSCFS.FSH]
mBlink False = [C0.ESC, SSCFS.STD]

-- | Enable or disable reverse video effect. No effect by default
mReverse :: Bool -> MString
mReverse True  = [C0.ESC, PSCFS.IPO]
mReverse False = [C0.ESC, PSCFS.NPO]

-- | Clear something on the screen relatively to the current cursor position
mClear :: WhatToClear -> MString
mClear Everything       = [C0.CS]
mClear EndOfLine        = [C0.CAN]
mClear EndOfScreen      = mCSI ANSI.ED [{- ANSI.CursorToEnd -}]
mClear StartOfScreen    = mCSI ANSI.ED [ANSI.BeginningToCursor]
mClear StartOfLine      = mCSI ANSI.EL [ANSI.BeginningToCursor]
mClear Line             = mCSI ANSI.EL [ANSI.AllCharacters]
mClear StatusLine       = mLocate 0 1 ++  mClear EndOfLine ++ [C0.APD]
mClear ReallyEverything = mClear Everything ++ mClear StatusLine

-- | Repeat a character. The Minitel has a special function for repeating
--   characters which helps save bandwidth.
mRepeat :: MNat -> MNat -> MString
mRepeat count c | count == 0  = []
                | count == 1  = [c]
                | count == 2  = [c, c]
                | count <  64 = [c, C0.RPT, 0x40 + count - 1]
                | otherwise   = error "Count is too big"

-- | Beep !!!
mBeep :: MString
mBeep = [ASCII.BEL]

-- | Removes a number of columns or rows on the Minitel screen. Note that
--   removing columns only concerns the current line. Removing row concerns
--   all columns. When removing a column, characters on its right are moved
--   on the left. When removing a row, characters below it are moved upward.
mRemove :: WhatToRemove -> MNat -> MString
mRemove Column count = mCSI ANSI.DCH [count]
mRemove Row    count = mCSI ANSI.DL  [count]

-- | Inserts a number of columns or rows on the Minitel screen. Note that
--   inserting columns only concerns the current line. Inserting row concerns
--   all columns. When inserting a column, characters on its right are moved
--   on the right. When inserting a row, characters below it are moved
--   downward.
mInsert :: WhatToInsert -> MNat -> MString
mInsert Column count = mCSI ANSI.SM [ANSI.IRM]
                    ++ replicate (fromMNat count) ASCII.Space
                    ++ mCSI ANSI.RM [ANSI.IRM]
mInsert Row    count = mCSI ANSI.IL [count]

-- | Enable or disable the semigraphic mode. Disabled by default.
mSemigraphic :: Bool -> MString
mSemigraphic True  = [C0.SO]
mSemigraphic False = [C0.SI]

-- | Chooses the character set to use
mUseSet :: CharSet -> MString
mUseSet G0  = sG0G0
mUseSet G1  = sG1G1
mUseSet G'0 = sG'0G0
mUseSet G'1 = sG'1G1

-- | Draw a rectangle on the Minitel Screen
mRectangle :: ToMColor a => MNat -> MNat -> MNat -> MNat -> a -> MString
mRectangle x y w h color = concat . concat $ map mLine [y .. (y + h - 1)]
    where mLine y' = [ mLocate x y'
                     , mBackground color
                     , mRepeat w ASCII.Space
                     ]
