{-|
Module      : Generator
DeESCription : MString generator
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module provides functions generating MString for the Minitel. It allows
you to generate special commands for the Minitel like clearing screen, cursor
position etc. without remembering the codes.
-}
module Minitel.Generate.Generator where

import           Minitel.Constants.Constants
import           Minitel.Type.MNatural
import           Minitel.Type.MString
import           Minitel.Type.Videotex

default (MNat)

-- | Convert a unicode string into an MString according to a mode. VideoTex
--   offers the best conversion.
mString :: MMode -> String -> MString
mString VideoTex s = concatMap toVideotex s
mString _ s = concatMap toTerminal s

-- | Set foreground color
mForeground :: (ToMColor a) => a -> MString
mForeground color = [eESC, 0x40 + toMColor color]

-- | Set background color
mBackground :: (ToMColor a) => a -> MString
mBackground color = [eESC, 0x50 + toMColor color]

-- | Move cursor to an absolute position. Rows and columns starts at 1
--   The 0 row is the status line (at the very top of the screen) and is
--   handled differently from the rest of the screen by the Minitel.
mLocate :: MNat -> MNat -> MString
mLocate 1 1 = [eAPH]
mLocate x y = [eAPA, 0x40 + y, 0x40 + x]

-- | Move cursor relatively to its current position. The function optimizes
--   the length of the generated MString
mMove :: Int -> Int -> MString
mMove 0 0 = []
mMove 0 y | y > 127 || y < -127 = error "y move too big"
          | y >= -4 && y <= -1  = replicate (abs y) eAPU
          | y >= 1  && y <=  4  = replicate y eAPD
          | y <  0              = sCSI ++ (showInt . mnat) (abs y) ++ [0x42]
          | y >  0              = sCSI ++ (showInt . mnat) y ++ [0x41]
mMove x 0 | x > 127 || x < -127 = error "x move too big"
          | x >= -4 && x <= -1  = replicate (abs x) eAPB
          | x >= 1  && x <=  4  = replicate x eAPF
          | x <  0              = sCSI ++ (showInt . mnat) (abs x) ++ [0x43]
          | x >  0              = sCSI ++ (showInt . mnat) x ++ [0x44]
mMove x y = mMove x 0 ++ mMove 0 y

-- | Move cursor to the start of the current line.
mGotoStartOfLine :: MString
mGotoStartOfLine = [eAPR]

-- | Change character size. The first argument is the width, the second the
--   height
mSize :: CharWidth -> CharHeight -> MString
mSize SimpleWidth SimpleHeight = [eESC, 0x4c + 0]
mSize SimpleWidth DoubleHeight = [eESC, 0x4c + 1]
mSize DoubleWidth SimpleHeight = [eESC, 0x4c + 2]
mSize DoubleWidth DoubleHeight = [eESC, 0x4c + 3]

-- | Enable or disable characters underscoring. No underscoring by default.
mUnderscore :: Bool -> MString
mUnderscore True  = [eESC, 0x5a]
mUnderscore False = [eESC, 0x59]

-- | Enable or disable characters blinking. No blinking by default.
mBlink :: Bool -> MString
mBlink True  = [eESC, 0x48]
mBlink False = [eESC, 0x49]

-- | Enable or disable reverse video effect. No effect by default
mReverse :: Bool -> MString
mReverse True  = [eESC, 0x5d]
mReverse False = [eESC, 0x5c]

-- | Clear something on the screen relatively to the current cursor position
mClear :: WhatToClear -> MString
mClear Everything       = [eCS]
mClear EndOfLine        = [eCAN]
mClear EndOfScreen      = sCSI ++ [0x4a]
mClear StartOfScreen    = sCSI ++ [0x31, 0x4a]
mClear StartOfLine      = sCSI ++ [0x31, 0x4b]
mClear Line             = sCSI ++ [0x32, 0x4b]
mClear StatusLine       = mLocate 0 1 ++  mClear EndOfLine ++ [eAPD]
mClear ReallyEverything = mClear Everything ++ mClear StatusLine

-- | Repeat a character. The Minitel has a special function for repeating
--   characters which helps save bandwidth.
mRepeat :: MNat -> MNat -> MString
mRepeat count c | count == 0  = []
                | count == 1  = [c]
                | count == 2  = [c, c]
                | count <  64 = [c, eRPT, 0x40 + count - 1]
                | otherwise   = error "Count is too big"

-- | Beep !!!
mBeep :: MString
mBeep = [aBEL]

-- | Removes a number of columns or rows on the Minitel screen. Note that
--   removing columns only concerns the current line. Removing row concerns
--   all columns. When removing a column, characters on its right are moved
--   on the left. When removing a row, characters below it are moved upward.
mRemove :: WhatToRemove -> MNat -> MString
mRemove Column count = sCSI ++ showInt count ++ [0x50]
mRemove Row    count = sCSI ++ showInt count ++ [0x4d]

-- | Inserts a number of columns or rows on the Minitel screen. Note that
--   inserting columns only concerns the current line. Inserting row concerns
--   all columns. When inserting a column, characters on its right are moved
--   on the right. When inserting a row, characters below it are moved
--   downward.
mInsert :: WhatToInsert -> MNat -> MString
mInsert Column count = sCSI ++ [0x34, 0x68] ++ replicate (fromMNat count) 0x20
                    ++ sCSI ++ [0x34, 0x6c]
mInsert Row    count = sCSI ++ showInt count ++ [0x4c]

-- | Enable or disable the semigraphic mode. Disabled by default.
mSemigraphic :: Bool -> MString
mSemigraphic True  = [eSO]
mSemigraphic False = [eSI]

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
                    , mRepeat w 0x20
                    ]
