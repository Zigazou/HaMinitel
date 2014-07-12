{-|
Module      : Generator
Description : MString generator
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module provides functions generating MString for the Minitel. It allows
you to generate special commands for the Minitel like clearing screen, cursor
position etc. without remembering the codes.
-}
module Minitel.Generator where

import Minitel.Constants
import Minitel.MString
import Data.Char

-- | The Minitel supports 3 modes, with VideoTex being the standard one with
--   semigraphic 40 columns. Terminal (TeleInformatique) mode is able to
--   display textual 80 columns.
data MMode = VideoTex | Mixed | Terminal
    deriving (Ord, Eq, Show)

-- | Though the Minitel is generally shipped with a monochrome screen, it
--   actually supports colors
type MColor = Int

-- | Type class helping to convert a color to the actual color code of the
--   Minitel
class ToMColor a where
    toMColor :: a -> MColor

-- | Colors the Minitel is able to render
data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving (Ord, Eq, Show)

-- | Grey scale the Minitel is able to render
data Grey = Grey0 | Grey1 | Grey2 | Grey3 | Grey4 | Grey5 | Grey6 | Grey7
    deriving (Ord, Eq, Show)

instance ToMColor Color where
    toMColor a = case a of
        Black   -> 0
        Red     -> 1
        Green   -> 2
        Yellow  -> 3
        Blue    -> 4
        Magenta -> 5
        Cyan    -> 6
        White   -> 7

instance ToMColor Grey where
    toMColor a = case a of
        Grey0 -> 0
        Grey1 -> 4
        Grey2 -> 1
        Grey3 -> 5
        Grey4 -> 2
        Grey5 -> 6
        Grey6 -> 3
        Grey7 -> 7

-- | Switch from one mode to another. Note that all modes combinations are not 
--   supported. For example, you cannot go directly from the Terminal mode to
--   the Mixed mode, you have to go back to VideoTex and then to Mixed.
--   Trying to go from a mode to the same mode will do nothing
mMode :: MMode -> MMode -> MConfirmation
mMode Terminal VideoTex = (csi ++ [0x3f, 0x7b], [sep, 0x5e])
mMode VideoTex Mixed    = (pro2 ++ mixed1     , [sep, 0x70])
mMode VideoTex Terminal = (pro2 ++ telinfo    , csi ++ [0x3f, 0x7a])
mMode Mixed    VideoTex = (pro2 ++ mixed2     , [sep, 0x71])
mMode Mixed    Terminal = mMode VideoTex Terminal
mMode _        _        = error "Unsupported mode switch"

-- | Convert a unicode string into an MString according to a mode. VideoTex
--   offers the best conversion.
mString :: MMode -> String -> MString
mString VideoTex s = concatMap toVideotex s
mString _ s = concatMap toTerminal s

-- | Get the identification MString from the Minitel
mIdentification :: MCall
mIdentification = (pro1 ++ [enqrom], 5)

-- | Enable or disable the extended keyboard. Disabled by default.
mExtendedKeyboard :: Bool -> MCall
mExtendedKeyboard True  = (pro3 ++ [start, recvKeyboard, extd], pro3Length)
mExtendedKeyboard False = (pro3 ++ [stop , recvKeyboard, extd], pro3Length)

-- | Enable or disable the cursor keys. If disabled, the user cannot use cursor
--   keys on its Minitel (they will beep). Disabled by default.
mCursorKeys :: Bool -> MCall
mCursorKeys True  = (pro3 ++ [start, recvKeyboard, c0], pro3Length)
mCursorKeys False = (pro3 ++ [stop , recvKeyboard, c0], pro3Length)

-- | Enable or disable caps lock mode. By default, the Minitel is in caps lock
--   mode.
mLowercaseKeyboard :: Bool -> MCall
mLowercaseKeyboard True  = (pro2 ++ [start, lowercase], pro2Length)
mLowercaseKeyboard False = (pro2 ++ [stop , lowercase], pro2Length)

-- | Set foreground color
mForeground :: (ToMColor a) => a -> MString
mForeground color = [esc, 0x40 + toMColor color]

-- | Set background color
mBackground :: (ToMColor a) => a -> MString
mBackground color = [esc, 0x50 + toMColor color]

-- | Move cursor to an absolute position. Rows and columns starts at 1
--   The 0 row is the status line (at the very top of the screen) and is
--   handled differently from the rest of the screen by the Minitel.
mLocate :: Int -> Int -> MString
mLocate 1 1 = [rs]
mLocate x y = [us, 0x40 + y, 0x40 + x]

-- | Move cursor relatively to its current position. The function optimizes
--   the length of the generated MString
mMove :: Int -> Int -> MString
mMove 0 0 = []
mMove 0 y | y >= -4 && y <= -1 = replicate (abs y) vt
          | y >= 1  && y <=  4 = replicate y lf
          | y <  0             = csi ++ showInt y ++ [0x42]
          | y >  0             = csi ++ showInt y ++ [0x41]
mMove x 0 | x >= -4 && x <= -1 = replicate (abs x) bs
          | x >= 1  && x <=  4 = replicate x tab
          | x <  0             = csi ++ showInt x ++ [0x43]
          | x >  0             = csi ++ showInt x ++ [0x44]
mMove x y = mMove x 0 ++ mMove 0 y

-- | Move cursor to the start of the current line.
mGotoStartOfLine :: MString
mGotoStartOfLine = [cr]

-- | The Minitel is able to double width or height of characters
data CharSize = SimpleSize | DoubleSize

-- | Change character size. The first argument is the width, the second the
--   height
mSize :: CharSize -> CharSize -> MString
mSize SimpleSize SimpleSize = [esc, 0x4c + 0]
mSize SimpleSize DoubleSize = [esc, 0x4c + 1]
mSize DoubleSize SimpleSize = [esc, 0x4c + 2]
mSize DoubleSize DoubleSize = [esc, 0x4c + 3]

-- | Enable or disable characters underscoring. No underscoring by default.
mUnderscore :: Bool -> MString
mUnderscore True  = [esc, 0x5a]
mUnderscore False = [esc, 0x59]

-- | Enable or disable characters blinking. No blinking by default.
mBlink :: Bool -> MString
mBlink True  = [esc, 0x48]
mBlink False = [esc, 0x49]

-- | Enable or disable reverse video effect. No effect by default
mReverse :: Bool -> MString
mReverse True  = [esc, 0x5d]
mReverse False = [esc, 0x5c]

-- | Display or hide the cursor. Hidden by default.
mVisibleCursor :: Bool -> MString
mVisibleCursor True  = [con]
mVisibleCursor False = [cof]

-- | Enable or disable the echoing of keys pressed on the keyboard. If enabled,
--   a key pressed is sent both at the screen and at us. The Minitel keyboard
--   allows the user to send control codes and thus change the Minitel
--   configuration. For example, it is possible to change character colors or
--   anything else.
mEchoing :: Bool -> MCall
mEchoing True  = (pro3 ++ [switchOn , recvScreen, sendModem], pro3Length)
mEchoing False = (pro3 ++ [switchOff, recvScreen, sendModem], pro3Length)

-- | Defines what can be cleared on the Minitel screen
data WhatToClear = Everything       -- ^ Clear everything except the status
                 | EndOfLine        -- ^ Clear from cursor to end of line
                 | EndOfScreen      -- ^ Clear from cursor to end of screen
                 | StartOfScreen    -- ^ Clear from cursor to start of screen
                 | StartOfLine      -- ^ Clear from cursor to start of line
                 | Line             -- ^ Clear the current line
                 | StatusLine       -- ^ Clear the status line
                 | ReallyEverything -- ^ Clear everything and the status line

-- | Clear something on the screen relatively to the current cursor position
mClear :: WhatToClear -> MString
mClear Everything       = [ff]
mClear EndOfLine        = [can]
mClear EndOfScreen      = csi ++ [0x4a]
mClear StartOfScreen    = csi ++ [0x31, 0x4a]
mClear StartOfLine      = csi ++ [0x31, 0x4b]
mClear Line             = csi ++ [0x32, 0x4b]
mClear StatusLine       = [us, 0x40, 0x41, can, lf]
mClear ReallyEverything = mClear Everything ++ mClear StatusLine

-- | Repeat a character. The Minitel has a special function for repeating
--   characters which helps save bandwidth.
mRepeat :: Int -> Int -> MString
mRepeat count c | count == 0  = []
                | count == 1  = [c]
                | count == 2  = [c, c]
                | count <  87 = [c, rep, 0x40 + count - 1]
                | otherwise   = error "Count is too big"

-- | Beep !!!
mBeep :: MString
mBeep = [bel]

-- | Defines what can be removed on the Minitel screen
data WhatToRemove = Column | Row

-- | Defines what can be inserted on the Minitel screen
type WhatToInsert = WhatToRemove

-- | Removes a number of columns or rows on the Minitel screen. Note that
--   removing columns only concerns the current line. Removing row concerns
--   all columns. When removing a column, characters on its right are moved
--   on the left. When removing a row, characters below it are moved upward.
mRemove :: WhatToRemove -> Int -> MString
mRemove Column count = csi ++ showInt count ++ [0x50]
mRemove Row    count = csi ++ showInt count ++ [0x4d]

-- | Inserts a number of columns or rows on the Minitel screen. Note that
--   inserting columns only concerns the current line. Inserting row concerns
--   all columns. When inserting a column, characters on its right are moved
--   on the right. When inserting a row, characters below it are moved
--   downward.
mInsert :: WhatToInsert -> Int -> MString
mInsert Column count = csi ++ [0x34, 0x68] ++ replicate count 0x20
                    ++ csi ++ [0x34, 0x6c]
mInsert Row    count = csi ++ showInt count ++ [0x4c]

-- | Enable or disable the semigraphic mode. Disabled by default.
mSemigraphic :: Bool -> MString
mSemigraphic True  = [so]
mSemigraphic False = [si]

-- | The Minitel uses two character sets: G0 and G1
data CharSet = G0 | G1

-- | Chooses which character set will be redefined. Useful only for the
--   mRedesign function.
mDefineSet :: CharSet -> MString
mDefineSet G0 = [us, 0x23, 0x20, 0x20, 0x20, 0x42, 0x49]
mDefineSet G1 = [us, 0x23, 0x20, 0x20, 0x20, 0x43, 0x49]

-- | Chooses the character set to use
mUseSet :: CharSet -> MString
mUseSet G0 = [esc, 0x28, 0x20, 0x42]
mUseSet G1 = [esc, 0x29, 0x20, 0x43]

-- | A character design is a list of 10 Strings. Each string contains 8
--   characters, either '1' or '0', thus allowing you to simple write the
--   character design using Haskell code.
type CharDesign = [String]

-- | Generate the MString used to redefine one character. Useful only for the
--   mRedesign function
mDesign :: CharDesign -> MString
mDesign design = map bits' ((multiSplit 6 . concat) design) ++ [0x30]
    where bits = foldl (\a v -> 2 * a + (if v == '1' then 1 else 0)) 0
          bits' s = 0x40 + bits s * (if length s == 2 then 16 else 1)
          multiSplit _ [] = []
          multiSplit nb s = start:multiSplit nb end
              where (start, end) = splitAt nb s

-- | Generate the MString used to redefine several characters. Useful only for
--   the mRedesign function
mDesigns :: [CharDesign] -> [MString]
mDesigns = map mDesign

-- | Redefine characters given the ord of the first character as the first
--   argument. The character set must also be indicated. It will be
--   automatically selected after the redefinition.
mRedesign :: Int -> [CharDesign] -> CharSet -> MString
mRedesign fromChar designs charset =
    mDefineSet charset
    ++ (concat . mDesigns) designs
    ++ [us, 0x41, 0x41]
    ++ mUseSet charset

