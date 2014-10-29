{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-|
Module      : Constants
Description : Values definitions from the Minitel universe
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

The names were translated from french into english (the original documentation
is in french).
-}
module Minitel.Constants.Constants where

import           Minitel.Type.MNatural

-- No need for Integer default values, they are overkill for the Minitel
default (MNat)

-- * ASCII-Minitel control codes
--   Minitel follows the ASCII control codes standard except for some values
--   (ie.: ASCII em = Minitel SS2)
nul = 0x00 -- null
soh = 0x01 -- start of heading
stx = 0x02 -- start of text
etx = 0x03 -- end of text
eot = 0x04 -- end of transmission
enq = 0x05 -- enquiry
ack = 0x06 -- acknowledge
bel = 0x07 -- bell
bs  = 0x08 -- backspace
tab = 0x09 -- horizontal tab
lf  = 0x0a -- line feed, new line
vt  = 0x0b -- vertical tab
ff  = 0x0c -- form feed, new page
cr  = 0x0d -- carriage return
so  = 0x0e -- shift out
si  = 0x0f -- shift in
dle = 0x10 -- data link escape
dc1 = 0x11 -- device control 1
con = 0x11 -- Cursor on
dc2 = 0x12 -- device control 2
rep = 0x12 -- Rep
dc3 = 0x13 -- device control 3
sep = 0x13 -- Sep
dc4 = 0x14 -- device control 4
cof = 0x14 -- Cursor off
nak = 0x15 -- negative acknowledge
syn = 0x16 -- synchronous idle
etb = 0x17 -- end of transmission block
can = 0x18 -- cancel
em  = 0x19 -- end of medium
ss2 = 0x19 -- SS2
sub = 0x1a -- substitute
esc = 0x1b -- escape
fs  = 0x1c -- file separator
gs  = 0x1d -- group separator
ss3 = 0x1d -- SS3
rs  = 0x1e -- record separator
us  = 0x1f -- unit separator

-- * Protocol sequences
pro1 = [esc, 0x39] -- protocol 1 (needs one more arguments)
pro2 = [esc, 0x3a] -- protocol 2 (needs two more arguments)
pro3 = [esc, 0x3b] -- protocol 3 (needs three more arguments)
csi  = [esc, 0x5b] -- CSI

-- * PRO1 commands
disconnection  = 0x67
connection     = 0x68
ret1           = 0x6c
ret2           = 0x6d
oppo           = 0x6f
statusTerminal = 0x70
statusKeyboard = 0x72
statusRunning  = 0x72
statusSpeed    = 0x74
statusProtocol = 0x76
enqrom         = 0x7b
reset          = 0x7f

-- * PRO2 commands
copy           = 0x7c
switchTo       = 0x62
noDiffusion    = 0x64
noAcknowledge  = 0x64
diffusion      = 0x65
acknowledge    = 0x65
transparency   = 0x66
start          = 0x69
stop           = 0x6a
prog           = 0x6b
answerKeyboard = 0x73
answerRunning  = 0x73
answerProtocol = 0x77
telinfo        = [0x31, 0x7d]
mixed1         = [0x32, 0x7d]
mixed2         = [0x32, 0x7e]

-- * PRO2+START-STOP codes
rollMode       = 0x43
procedure      = 0x44
lowercase      = 0x45

-- * PRO2+PROG codes
b9600          = 0x7f -- 9600 baud
b4800          = 0x76 -- 4800 baud
b1200          = 0x64 -- 1200 baud
b300           = 0x52 -- 300 baud

-- * PRO3 Commands
switchOff      = 0x60
switchOn       = 0x61
switchFrom     = 0x63

-- * PRO commands length
pro1Length     = 3
pro2Length     = 4
pro3Length     = 5

-- * Other codes
copyFrench     = 0x6a
copyAmerican   = 0x6b
extd           = 0x41
c0             = 0x43

-- * Reception codes
recvScreen     = 0x58
recvKeyboard   = 0x59
recvModem      = 0x5a
recvPlug       = 0x5b
recvPhone      = 0x5c
recvSoftware   = 0x5d

-- * Emission codes
sendScreen     = 0x50
sendKeyboard   = 0x51
sendModem      = 0x52
sendPlug       = 0x53
sendPhone      = 0x54
sendSoftware   = 0x55

-- * Accents (for the VideoTex mode)
accCedilla     = [ss2, 0x4b]
accGrave       = [ss2, 0x41]
accAcute       = [ss2, 0x42]
accCirconflexe = [ss2, 0x43]
accUmlaut      = [ss2, 0x48]

-- * Direction keys
keyUp          = [0x0b]
keyDown        = [0x0a]
keyLeft        = [0x08]
keyRight       = [0x09]

keyShiftUp    = csi ++ [0x4d]
keyShiftDown  = csi ++ [0x4c]
keyShiftLeft  = csi ++ [0x50]
keyShiftRight = csi ++ [0x34, 0x68]

ctrlLeft = [0x7f]

-- * Return key
keyReturn      = [0x0d]
keyShiftReturn = csi ++ [0x48]
keyCtrlReturn  = csi ++ [0x32, 0x4a]

-- * Function keys
keyFuncSend       = [dc3, 0x41]
keyFuncPrev       = [dc3, 0x42]
keyFuncRepeat     = [dc3, 0x43]
keyFuncGuide      = [dc3, 0x44]
keyFuncCancel     = [dc3, 0x45]
keyFuncTOC        = [dc3, 0x46]
keyFuncCorrection = [dc3, 0x47]
keyFuncNext       = [dc3, 0x48]
keyFuncConnection = [dc3, 0x49]
