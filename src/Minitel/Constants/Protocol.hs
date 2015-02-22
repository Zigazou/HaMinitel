{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Protocol
Description : Protocol 1, 2, 3 constants
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Protocol 1, 2, 3 constants.
-}
module Minitel.Constants.Protocol where

import qualified Minitel.Constants.C0 as C0
import Minitel.Type.MNatural (MNat)

-- * Protocol sequences
pattern PRO1 x     = [C0.ESC, 0x39, x      ] -- ^ PROtocol 1 (1 argument)
pattern PRO2 x y   = [C0.ESC, 0x3a, x, y   ] -- ^ PROtocol 2 (2 arguments)
pattern PRO3 x y z = [C0.ESC, 0x3b, x, y, z] -- ^ PROtocol 3 (3 arguments)

-- * PRO1 commands
pattern Disconnection  = 0x67
pattern Connection     = 0x68
pattern Ret1           = 0x6c
pattern Ret2           = 0x6d
pattern Oppo           = 0x6f
pattern StatusTerminal = 0x70
pattern StatusKeyboard = 0x72
pattern StatusRunning  = 0x72
pattern StatusSpeed    = 0x74
pattern StatusProtocol = 0x76
pattern EnqROM         = 0x7b
pattern Reset          = 0x7f

-- * PRO2 commands
pattern Copy           = 0x7c
pattern SwitchTo       = 0x62
pattern NoDiffusion    = 0x64
pattern NoAcknowledge  = 0x64
pattern Diffusion      = 0x65
pattern Acknowledge    = 0x65
pattern Transparency   = 0x66
pattern Start          = 0x69
pattern Stop           = 0x6a
pattern Prog           = 0x6b
pattern AnswerKeyboard = 0x73
pattern AnswerRunning  = 0x73
pattern AnswerProtocol = 0x77
pattern TelinfoA       = 0x31
pattern TelinfoB       = 0x7d
pattern Mixed1A        = 0x32
pattern Mixed1B        = 0x7d
pattern Mixed2A        = 0x32
pattern Mixed2B        = 0x7e

-- * PRO2+START-STOP codes
pattern RollMode       = 0x43
pattern Procedure      = 0x44
pattern Lowercase      = 0x45
pattern ZoomUpper      = 0x46
pattern ZoomLower      = 0x47

-- * PRO2+PROG codes
pattern B9600          = 0x7f -- 9600 baud
pattern B4800          = 0x76 -- 4800 baud
pattern B1200          = 0x64 -- 1200 baud
pattern B300           = 0x52 -- 300 baud

-- * PRO3 Commands
pattern SwitchOff      = 0x60
pattern SwitchOn       = 0x61
pattern SwitchFrom     = 0x63

-- * PRO commands length
pro1Length, pro2Length, pro3Length :: MNat
pro1Length     = 3
pro2Length     = 4
pro3Length     = 5

-- * Other codes
pattern CopyFrench     = 0x6a
pattern CopyAmerican   = 0x6b
pattern Extd           = 0x41
pattern PC0            = 0x43

-- * Reception codes
pattern RecvScreen     = 0x58
pattern RecvKeyboard   = 0x59
pattern RecvModem      = 0x5a
pattern RecvPlug       = 0x5b
pattern RecvPhone      = 0x5c
pattern RecvSoftware   = 0x5d

-- * Emission codes
pattern SendScreen     = 0x50
pattern SendKeyboard   = 0x51
pattern SendModem      = 0x52
pattern SendPlug       = 0x53
pattern SendPhone      = 0x54
pattern SendSoftware   = 0x55

