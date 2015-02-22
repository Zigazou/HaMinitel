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
module Minitel.Generate.Configuration
( mMode
, mIdentification
, minitelInfo
, mExtendedKeyboard
, mCursorKeys
, mLowercaseKeyboard
, mVisibleCursor
, mEchoing
, mSpeed
, mRollMode
, mZoomUpper
, mZoomLower
)
where

import Minitel.Constants.Constants
import Minitel.Type.MNatural (MNat, fromMNat)
import Minitel.Type.MString (MString, MConfirmation, MCall)
import Minitel.Type.Videotex (MMode (Terminal, VideoTex, Mixed))
import Minitel.Type.Ability (Ability (abilityId), Maker (makerId))
import Minitel.Constants.Abilities (minitelAbilities, makers)
import Data.Char (chr)
import Data.List (find)

default (MNat)

-- | Switch from one mode to another. Note that all modes combinations are not
--   supported. For example, you cannot go directly from the Terminal mode to
--   the Mixed mode, you have to go back to VideoTex and then to Mixed.
--   Trying to go from a mode to the same mode will do nothing
mMode :: MMode -> MMode -> MConfirmation
mMode Terminal VideoTex = (sCSI ++ [0x3f, 0x7b], [aDC3, 0x5e])
mMode VideoTex Mixed    = (sPRO2 ++ mixed1     , [aDC3, 0x70])
mMode VideoTex Terminal = (sPRO2 ++ telinfo    , sCSI ++ [0x3f, 0x7a])
mMode Mixed    VideoTex = (sPRO2 ++ mixed2     , [aDC3, 0x71])
mMode Mixed    Terminal = mMode VideoTex Terminal
mMode _        _        = error "Unsupported mode switch"

-- | Get the identification MString from the Minitel
mIdentification :: MCall
mIdentification = (sPRO1 ++ [enqrom], 5)

-- | Translate the identification code to a human version
chrm :: MNat -> Char
chrm = chr . fromMNat

findAbility :: MNat -> Maybe Ability
findAbility model = find (\a -> abilityId a == chrm model) minitelAbilities

findMaker :: MNat -> Maybe Maker
findMaker maker = find (\m -> makerId m == chrm maker) makers

minitelInfo :: MString -> (Maybe Maker, Maybe Ability, Char)
minitelInfo [0x01, maker, model, number, 0x04] =
    ( findMaker maker
    , findAbility model
    , chrm number
    )
minitelInfo _ = (Nothing, Nothing, '0')

-- | Enable or disable the extended keyboard. Disabled by default.
mExtendedKeyboard :: Bool -> MCall
mExtendedKeyboard True  = (sPRO3 ++ [start, recvKeyboard, extd], pro3Length)
mExtendedKeyboard False = (sPRO3 ++ [stop , recvKeyboard, extd], pro3Length)

-- | Enable or disable the cursor keys. If disabled, the user cannot use cursor
--   keys on its Minitel (they will beep). Disabled by default.
mCursorKeys :: Bool -> MCall
mCursorKeys True  = (sPRO3 ++ [start, recvKeyboard, c0], pro3Length)
mCursorKeys False = (sPRO3 ++ [stop , recvKeyboard, c0], pro3Length)

-- | Enable or disable a functionality using PRO2 protocol.
mPRO2Toggle :: MNat -> Bool -> MCall
mPRO2Toggle mn True  = (sPRO2 ++ [start, mn], pro2Length)
mPRO2Toggle mn False = (sPRO2 ++ [stop , mn], pro2Length)

-- | Enable or disable caps lock mode. By default, the Minitel is in caps lock
--   mode.
mLowercaseKeyboard :: Bool -> MCall
mLowercaseKeyboard = mPRO2Toggle lowercase

-- | Enable or disable roll mode. By default, the roll mode is disabled
--   When disabled, if the user tries to write past the bottom right character,
--   it will start to write from the top left character. When enabled, it acts
--   like a standard terminal.
mRollMode :: Bool -> MCall
mRollMode = mPRO2Toggle rollMode

-- | Enable or disable zoom on the upper part of the Minitel (available only on
--   Minitel 1, not 1B etc.)
mZoomUpper :: Bool -> MCall
mZoomUpper = mPRO2Toggle zoomUpper

-- | Enable or disable zoom on the lower part of the Minitel (available only on
--   Minitel 1, not 1B etc.)
mZoomLower :: Bool -> MCall
mZoomLower = mPRO2Toggle zoomLower

-- | Display or hide the cursor. Hidden by default.
mVisibleCursor :: Bool -> MString
mVisibleCursor True  = [eCON]
mVisibleCursor False = [eCOF]

-- | Enable or disable the echoing of keys pressed on the keyboard. If enabled,
--   a key pressed is sent both at the screen and at us. The Minitel keyboard
--   allows the user to send control codes and thus change the Minitel
--   configuration. For example, it is possible to change character colors or
--   anything else.
mEchoing :: Bool -> MCall
mEchoing True  = (sPRO3 ++ [switchOn , recvScreen, sendModem], pro3Length)
mEchoing False = (sPRO3 ++ [switchOff, recvScreen, sendModem], pro3Length)

-- | Change Minitel speed
mSpeed :: Int -> MCall
mSpeed 300  = (sPRO2 ++ [prog, b300 ], pro2Length)
mSpeed 1200 = (sPRO2 ++ [prog, b1200], pro2Length)
mSpeed 4800 = (sPRO2 ++ [prog, b4800], pro2Length)
mSpeed 9600 = (sPRO2 ++ [prog, b9600], pro2Length)
mSpeed _    = error "Unsupported speed"
