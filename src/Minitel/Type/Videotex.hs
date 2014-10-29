{-|
Module      : Videotex
Description : 
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

-}
module Minitel.Type.Videotex where

import Minitel.Type.MNatural

default (MNat)

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

-- | Defines what can be removed on the Minitel screen
data WhatToRemove = Column | Row

-- | Defines what can be inserted on the Minitel screen
type WhatToInsert = WhatToRemove

-- | The Minitel uses four character sets: G0 and G1
data CharSet = G0 | G1 | G'0 | G'1

-- | The Minitel is able to double width or height of characters
data CharWidth = SimpleWidth | DoubleWidth deriving Show
data CharHeight = SimpleHeight | DoubleHeight deriving Show

-- | Defines what can be cleared on the Minitel screen
data WhatToClear = Everything       -- ^ Clear everything except the status
                 | EndOfLine        -- ^ Clear from cursor to end of line
                 | EndOfScreen      -- ^ Clear from cursor to end of screen
                 | StartOfScreen    -- ^ Clear from cursor to start of screen
                 | StartOfLine      -- ^ Clear from cursor to start of line
                 | Line             -- ^ Clear the current line
                 | StatusLine       -- ^ Clear the status line
                 | ReallyEverything -- ^ Clear everything and the status line

-- | The Minitel supports 3 modes, with VideoTex being the standard one with
--   semigraphic 40 columns. Terminal (TeleInformatique) mode is able to
--   display textual 80 columns.
data MMode = VideoTex | Mixed | Terminal
    deriving (Ord, Eq, Show)
