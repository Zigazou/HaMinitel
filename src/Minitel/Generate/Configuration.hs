{-|
Module      : Configuration
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
module Minitel.Generate.Configuration where

import           Minitel.Constants.Constants
import           Minitel.Type.MNatural
import           Minitel.Type.MString
import           Minitel.Type.Videotex

default (MNat)

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

-- | Change Minitel speed
mSpeed :: Int -> MCall
mSpeed 300  = (pro2 ++ [prog, b300 ], pro2Length)
mSpeed 1200 = (pro2 ++ [prog, b1200], pro2Length)
mSpeed 4800 = (pro2 ++ [prog, b4800], pro2Length)
mSpeed 9600 = (pro2 ++ [prog, b9600], pro2Length)
mSpeed _    = error "Unsupported speed"
