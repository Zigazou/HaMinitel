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

import           Data.Char
import           Data.List.Split   (splitEvery)
import           Minitel.Constants
import           Minitel.MNatural
import           Minitel.MString

default (MNat)

-- | The Minitel supports 3 modes, with VideoTex being the standard one with
--   semigraphic 40 columns. Terminal (TeleInformatique) mode is able to
--   display textual 80 columns.
data MMode = VideoTex | Mixed | Terminal
    deriving (Ord, Eq, Show)

-- | Though the Minitel is generally shipped with a monochrome screen, it
--   actually supports colors
type MColor = MNat

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
mLocate :: MNat -> MNat -> MString
mLocate 1 1 = [rs]
mLocate x y = [us, 0x40 + y, 0x40 + x]

-- | Move cursor relatively to its current position. The function optimizes
--   the length of the generated MString
mMove :: Int -> Int -> MString
mMove 0 0 = []
mMove 0 y | y > 127 || y < 127 = error "y move too big"
          | y >= -4 && y <= -1 = replicate (abs y) vt
          | y >= 1  && y <=  4 = replicate y lf
          | y <  0             = csi ++ (showInt . toMNat) y ++ [0x42]
          | y >  0             = csi ++ (showInt . toMNat) y ++ [0x41]
mMove x 0 | x > 127 || x < 127 = error "x move too big"
          | x >= -4 && x <= -1 = replicate (abs x) bs
          | x >= 1  && x <=  4 = replicate x tab
          | x <  0             = csi ++ (showInt . toMNat) x ++ [0x43]
          | x >  0             = csi ++ (showInt . toMNat) x ++ [0x44]
mMove x y = mMove x 0 ++ mMove 0 y

-- | Move cursor to the start of the current line.
mGotoStartOfLine :: MString
mGotoStartOfLine = [cr]

-- | The Minitel is able to double width or height of characters
data CharWidth = SimpleWidth | DoubleWidth deriving Show
data CharHeight = SimpleHeight | DoubleHeight deriving Show

-- | Change character size. The first argument is the width, the second the
--   height
mSize :: CharWidth -> CharHeight -> MString
mSize SimpleWidth SimpleHeight = [esc, 0x4c + 0]
mSize SimpleWidth DoubleHeight = [esc, 0x4c + 1]
mSize DoubleWidth SimpleHeight = [esc, 0x4c + 2]
mSize DoubleWidth DoubleHeight = [esc, 0x4c + 3]

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
mRepeat :: MNat -> MNat -> MString
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
mRemove :: WhatToRemove -> MNat -> MString
mRemove Column count = csi ++ showInt count ++ [0x50]
mRemove Row    count = csi ++ showInt count ++ [0x4d]

-- | Inserts a number of columns or rows on the Minitel screen. Note that
--   inserting columns only concerns the current line. Inserting row concerns
--   all columns. When inserting a column, characters on its right are moved
--   on the right. When inserting a row, characters below it are moved
--   downward.
mInsert :: WhatToInsert -> MNat -> MString
mInsert Column count = csi ++ [0x34, 0x68] ++ replicate (fromMNat count) 0x20
                    ++ csi ++ [0x34, 0x6c]
mInsert Row    count = csi ++ showInt count ++ [0x4c]

-- | Enable or disable the semigraphic mode. Disabled by default.
mSemigraphic :: Bool -> MString
mSemigraphic True  = [so]
mSemigraphic False = [si]

-- | The Minitel uses two character sets: G0 and G1
data CharSet = G0 | G1 | G'0 | G'1

-- | Chooses which character set will be redefined. Useful only for the
--   mRedesign function.
mDefineSet :: CharSet -> MString
mDefineSet G'0 = [us, 0x23, 0x20, 0x20, 0x20, 0x42, 0x49]
mDefineSet G'1 = [us, 0x23, 0x20, 0x20, 0x20, 0x43, 0x49]
mDefineSet _   = error "G0 or G1 charsets cannot be redefined"

-- | Chooses the character set to use
mUseSet :: CharSet -> MString
mUseSet G0  = [esc, 0x28, 0x40]
mUseSet G1  = [esc, 0x29, 0x63]
mUseSet G'0 = [esc, 0x28, 0x20, 0x42]
mUseSet G'1 = [esc, 0x29, 0x20, 0x43]

-- | A character design is a list of 10 Strings. Each string contains 8
--   characters, either '1' or '0', thus allowing you to simple write the
--   character design using Haskell code.
type CharDesign = [String]

-- | Generate the MString used to redefine one character. Useful only for the
--   mRedesign function
mDesign :: CharDesign -> MString
mDesign design = map bitsToMtel ((splitEvery 6 . concat) design) ++ [0x30]
    where bitsToNat = foldl (\a v -> 2 * a + (if v == '1' then 1 else 0)) 0
          bitsToMtel s = 0x40 + bitsToNat s * (if length s == 2 then 16 else 1)

-- | Generate the MString used to redefine several characters. Useful only for
--   the mRedesign function
mDesigns :: [CharDesign] -> [MString]
mDesigns = map mDesign

-- | Redefine characters given the ord of the first character as the first
--   argument. The character set must also be indicated. It will be
--   automatically selected after the redefinition.
mRedesign :: MNat -> [CharDesign] -> CharSet -> MString
mRedesign fromChar designs charset =
    mDefineSet charset
    ++ [us, 0x23, fromChar, 0x30]
    ++ (concat . mDesigns) designs
    ++ [us, 0x41, 0x41]
    ++ mUseSet charset

-- | Draw a rectangle on the Minitel Screen
mRectangle :: ToMColor a => MNat
                         -> MNat
                         -> MNat
                         -> MNat
                         -> a
                         -> MString
mRectangle x y w h color = concat . concat $ map mLine [y .. (y + h - 1)]
    where mLine y = [ mLocate x y
                    , mBackground color
                    , mRepeat w 0x20
                    ]

-- | Change Minitel speed
mSpeed :: Int -> MCall
mSpeed 300  = (pro2 ++ [prog, b300 ], pro2Length)
mSpeed 1200 = (pro2 ++ [prog, b1200], pro2Length)
mSpeed 4800 = (pro2 ++ [prog, b4800], pro2Length)
mSpeed 9600 = (pro2 ++ [prog, b9600], pro2Length)
mSpeed _    = error "Unsupported speed"
