{-# LANGUAGE PatternSynonyms #-}
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

import Minitel.Constants.CSI
import Minitel.Constants.Protocol
import qualified Minitel.Constants.ASCII    as ASCII
import qualified Minitel.Constants.C0       as C0
import Minitel.Type.MNatural (MNat, chrm)
import Minitel.Type.MString (MString, MConfirmation, MCall)
import Minitel.Type.Videotex (MMode (Terminal, VideoTex, Mixed))
import Minitel.Type.Ability (Ability (abilityId), Maker (makerId))
import Minitel.Constants.Abilities (minitelAbilities, makers)
import Data.List (find)

default (MNat)

-- | Switch from one mode to another. Note that all modes combinations are not
--   supported. For example, you cannot go directly from the Terminal mode to
--   the Mixed mode, you have to go back to VideoTex and then to Mixed.
--   Trying to go from a mode to the same mode will do nothing
mMode :: MMode -> MMode -> MConfirmation
mMode Terminal VideoTex = (CSI2 0x3f 0x7b        , [ASCII.DC3, 0x5e])
mMode VideoTex Mixed    = (PRO2 Mixed1A Mixed1B  , [ASCII.DC3, 0x70])
mMode VideoTex Terminal = (PRO2 TelinfoA TelinfoB, CSI2 0x3f 0x7a   )
mMode Mixed    VideoTex = (PRO2 Mixed2A Mixed2B  , [ASCII.DC3, 0x71])
mMode Mixed    Terminal = mMode VideoTex Terminal
mMode _        _        = error "Unsupported mode switch"

-- | Get the identification MString from the Minitel
mIdentification :: MCall
mIdentification = (PRO1 EnqROM, 5)

findAbility :: MNat -> Maybe Ability
findAbility model = find (\a -> abilityId a == chrm model) minitelAbilities

findMaker :: MNat -> Maybe Maker
findMaker maker = find (\m -> makerId m == chrm maker) makers

minitelInfo :: MString -> (Maybe Maker, Maybe Ability, Char)
minitelInfo [ASCII.SOH, maker, model, number, ASCII.EOT] =
    ( findMaker maker
    , findAbility model
    , chrm number
    )
minitelInfo _ = (Nothing, Nothing, '0')

-- | Enable or disable the extended keyboard. Disabled by default.
mExtendedKeyboard :: Bool -> MCall
mExtendedKeyboard True  = (PRO3 Start RecvKeyboard Extd, pro3Length)
mExtendedKeyboard False = (PRO3 Stop  RecvKeyboard Extd, pro3Length)

-- | Enable or disable the cursor keys. If disabled, the user cannot use cursor
--   keys on its Minitel (they will beep). Disabled by default.
mCursorKeys :: Bool -> MCall
mCursorKeys True  = (PRO3 Start RecvKeyboard PC0, pro3Length)
mCursorKeys False = (PRO3 Stop  RecvKeyboard PC0, pro3Length)

-- | Enable or disable a functionality using PRO2 protocol.
mPRO2Toggle :: MNat -> Bool -> MCall
mPRO2Toggle mn True  = (PRO2 Start mn, pro2Length)
mPRO2Toggle mn False = (PRO2 Stop  mn, pro2Length)

-- | Enable or disable caps lock mode. By default, the Minitel is in caps lock
--   mode.
mLowercaseKeyboard :: Bool -> MCall
mLowercaseKeyboard = mPRO2Toggle Lowercase

-- | Enable or disable roll mode. By default, the roll mode is disabled
--   When disabled, if the user tries to write past the bottom right character,
--   it will start to write from the top left character. When enabled, it acts
--   like a standard terminal.
mRollMode :: Bool -> MCall
mRollMode = mPRO2Toggle RollMode

-- | Enable or disable zoom on the upper part of the Minitel (available only on
--   Minitel 1, not 1B etc.)
mZoomUpper :: Bool -> MCall
mZoomUpper = mPRO2Toggle ZoomUpper

-- | Enable or disable zoom on the lower part of the Minitel (available only on
--   Minitel 1, not 1B etc.)
mZoomLower :: Bool -> MCall
mZoomLower = mPRO2Toggle ZoomLower

-- | Display or hide the cursor. Hidden by default.
mVisibleCursor :: Bool -> MString
mVisibleCursor True  = [C0.CON]
mVisibleCursor False = [C0.COF]

-- | Enable or disable the echoing of keys pressed on the keyboard. If enabled,
--   a key pressed is sent both at the screen and at us. The Minitel keyboard
--   allows the user to send control codes and thus change the Minitel
--   configuration. For example, it is possible to change character colors or
--   anything else.
mEchoing :: Bool -> MCall
mEchoing True  = (PRO3 SwitchOn  RecvScreen SendModem, pro3Length)
mEchoing False = (PRO3 SwitchOff RecvScreen SendModem, pro3Length)

-- | Change Minitel speed
mSpeed :: Int -> MCall
mSpeed 300  = (PRO2 Prog B300 , pro2Length)
mSpeed 1200 = (PRO2 Prog B1200, pro2Length)
mSpeed 4800 = (PRO2 Prog B4800, pro2Length)
mSpeed 9600 = (PRO2 Prog B9600, pro2Length)
mSpeed _    = error "Unsupported speed"

