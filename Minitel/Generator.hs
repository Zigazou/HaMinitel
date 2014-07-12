-- Module Generator
module Minitel.Generator where

import Minitel.Constants
import Minitel.MString
import Data.Char

data MMode = VideoTex | Mixed | Terminal
    deriving (Ord, Eq, Show)

type MColor = Integer

class ToMColor a where
    toMColor :: a -> MColor

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving (Ord, Eq, Show)

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

instance Read Color where
    readsPrec _ s
        | s == "black"   = [(Black, "black")]
        | s == "red"     = [(Red, "red")]
        | s == "green"   = [(Green, "green")]
        | s == "yellow"  = [(Yellow, "yellow")]
        | s == "blue"    = [(Blue, "blue")]
        | s == "magenta" = [(Magenta, "magenta")]
        | s == "cyan"    = [(Cyan, "cyan")]
        | s == "white"   = [(White, "white")]
        | otherwise = []

mMode :: MMode -> MMode -> MConfirmation
mMode Terminal VideoTex = (csi ++ [0x3f, 0x7b], [sep, 0x5e])
mMode VideoTex Mixed    = (pro2 ++ mixed1, [sep, 0x70])
mMode VideoTex Terminal = (pro2 ++ telinfo, csi ++ [0x3f, 0x7a])
mMode Mixed    VideoTex = (pro2 ++ mixed2, [sep, 0x71])
mMode Mixed    Terminal = mMode VideoTex Terminal
mMode _        _        = error "Unsupported mode switch"

mString :: MMode -> String -> MString
mString VideoTex s = concatMap toVideotex s
mString _ s = concatMap toTerminal s

mIdentification :: MCall
mIdentification = (pro1 ++ [enqrom], 5)

mExtendedKeyboard :: Bool -> MCall
mExtendedKeyboard True  = (pro3 ++ [start, rcptKeyboard, eten], pro3Length)
mExtendedKeyboard False = (pro3 ++ [stop , rcptKeyboard, eten], pro3Length)

mCursorKeys :: Bool -> MCall
mCursorKeys True  = (pro3 ++ [start, rcptKeyboard, c0], pro3Length)
mCursorKeys False = (pro3 ++ [stop , rcptKeyboard, c0], pro3Length)

mLowercaseKeyboard :: Bool -> MCall
mLowercaseKeyboard True  = (pro2 ++ [start, lowercase], pro2Length)
mLowercaseKeyboard False = (pro2 ++ [stop , lowercase], pro2Length)

mForeground :: (ToMColor a) => a -> MString
mForeground color = [esc, 0x40 + toMColor color]

mBackground :: (ToMColor a) => a -> MString
mBackground color = [esc, 0x50 + toMColor color]

mLocate :: Integer -> Integer -> MString
mLocate 1 1 = [rs]
mLocate x y = [us, 0x40 + y, 0x40 + x]

mLocateR :: Integer -> Integer -> MString
mLocateR 0 0 = []
mLocateR 0 y
    | y >= -4 && y <= -1 = replicate (fromIntegral . abs $ y) vt
    | y >= 1  && y <=  4 = replicate (fromIntegral y) lf
    | y <  0             = csi ++ showInteger y ++ [0x42]
    | y >  0             = csi ++ showInteger y ++ [0x41]
mLocateR x 0
    | x >= -4 && x <= -1 = replicate (fromIntegral . abs $ x) bs
    | x >= 1  && x <=  4 = replicate (fromIntegral x) tab
    | x <  0             = csi ++ showInteger x ++ [0x43]
    | x >  0             = csi ++ showInteger x ++ [0x44]
mLocateR x y = mLocateR x 0 ++ mLocateR 0 y

mSize :: Integer -> Integer -> MString
mSize width height = [esc, 0x4c + (height - 1) + (width - 1) * 2]

mUnderscore :: Bool -> MString
mUnderscore True  = [esc, 0x5a]
mUnderscore False = [esc, 0x59]

mBlink :: Bool -> MString
mBlink True  = [esc, 0x48]
mBlink False = [esc, 0x49]

mReverse :: Bool -> MString
mReverse True  = [esc, 0x5d]
mReverse False = [esc, 0x5c]

mVisibleCursor :: Bool -> MString
mVisibleCursor True  = [con]
mVisibleCursor False = [cof]

mEchoing :: Bool -> MCall
mEchoing True  = (pro3 ++ [switchOn , rcptScreen, emitModem], pro3Length)
mEchoing False = (pro3 ++ [switchOff, rcptScreen, emitModem], pro3Length)

data WhatToClear = Everything
                 | EndOfLine
                 | EndOfScreen
                 | StartOfScreen
                 | StartOfLine
                 | Line
                 | StatusLine
                 | ReallyEverything

mClear :: WhatToClear -> MString
mClear Everything       = [ff]
mClear EndOfLine        = [can]
mClear EndOfScreen      = csi ++ [0x4a]
mClear StartOfScreen    = csi ++ [0x31, 0x4a]
mClear StartOfLine      = csi ++ [0x31, 0x4b]
mClear Line             = csi ++ [0x32, 0x4b]
mClear StatusLine       = [us, 0x40, 0x41, can, lf]
mClear ReallyEverything = mClear Everything ++ mClear StatusLine

mRepeat :: Integer -> Integer -> MString
mRepeat count c = [c, rep, 0x40 + count - 1]

mBeep :: MString
mBeep = [bel]

mGotoStartOfLine :: MString
mGotoStartOfLine = [cr]

data WhatToRemove = Column | Row
type WhatToInsert = WhatToRemove

mRemove :: WhatToRemove -> Integer -> MString
mRemove Column count = csi ++ showInteger count ++ [0x50]
mRemove Row    count = csi ++ showInteger count ++ [0x4d]

mInsert :: WhatToInsert -> Integer -> MString
mInsert Column count = csi ++ [0x34, 0x68]
                    ++ replicate (fromIntegral count) 0x20
                    ++ csi ++ [0x34, 0x6c]
mInsert Row    count = csi ++ showInteger count ++ [0x4c]

mSemigraphic :: Bool -> MString
mSemigraphic True  = [so]
mSemigraphic False = [si]

-- Character redefinition
type CharDesign = [String]
data CharSet = G0 | G1

mDefineSet :: CharSet -> MString
mDefineSet G0 = [us, 0x23, 0x20, 0x20, 0x20, 0x42, 0x49]
mDefineSet G1 = [us, 0x23, 0x20, 0x20, 0x20, 0x43, 0x49]

mUseSet :: CharSet -> MString
mUseSet G0 = [esc, 0x28, 0x20, 0x42]
mUseSet G1 = [esc, 0x29, 0x20, 0x43]

mDesign :: CharDesign -> MString
mDesign design = map bits' ((multiSplit 6 . concat) design) ++ [0x30]
    where bits = foldl (\a v -> 2 * a + (if v == '1' then 1 else 0)) 0
          bits' s = 0x40 + bits s * (if length s == 2 then 16 else 1)
          multiSplit _ [] = []
          multiSplit nb s = start:multiSplit nb end
              where (start, end) = splitAt nb s

mDesigns :: [CharDesign] -> [MString]
mDesigns = map mDesign

mRedesign :: Integer -> [CharDesign] -> CharSet -> MString
mRedesign fromChar designs charset =
    mDefineSet charset
    ++ (concat . mDesigns) designs
    ++ [us, 0x41, 0x41]
    ++ mUseSet charset

