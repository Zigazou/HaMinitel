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

import Minitel.Type.MNatural (MNat)

-- No need for Integer default values, they are overkill for the Minitel
default (MNat)

-- * ASCII control codes
aNUL = 0x00 -- Null
aSOH = 0x01 -- Start Of Heading
aSTX = 0x02 -- Start of TeXt
aETX = 0x03 -- End of TeXt
aEOT = 0x04 -- End Of Transmission
aENQ = 0x05 -- ENQuiry
aACK = 0x06 -- ACKnowledge
aBEL = 0x07 -- BELl
aBS  = 0x08 -- BackSpace
aHT  = 0x09 -- Horizontal Tab
aLF  = 0x0a -- Line Feed / new line
aVT  = 0x0b -- Vertical Tab
aFF  = 0x0c -- Form Feed
aCR  = 0x0d -- Carriage Return
aSO  = 0x0e -- Shift Out
aSI  = 0x0f -- Shift In
aDLE = 0x10 -- Data Link Escape
aDC1 = 0x11 -- Device Control 1
aDC2 = 0x12 -- Device Control 2
aDC3 = 0x13 -- Device Control 3
aDC4 = 0x14 -- Device Control 4
aNAK = 0x15 -- Negative AcKnowledge
aSYN = 0x16 -- SYNchronous idle
aETB = 0x17 -- End of Transmission Block
aCAN = 0x18 -- CANcel
aEM  = 0x19 -- End of Medium
aSUB = 0x1a -- SUBstitute
aESC = 0x1b -- ESCape
aFS  = 0x1c -- File Separator
aGS  = 0x1d -- Group Separator
aRS  = 0x1e -- Record Separator
aUS  = 0x1f -- Unit Separator

-- * Primary control function set (default CO set) - ETS 300 072
eNUL = 0x00 -- NULl
eAPB = 0x08 -- Active Position Back
eAPF = 0x09 -- Active Position Forward
eAPD = 0x0a -- Active Position Down
eAPU = 0x0b -- Active Position Up
eCS  = 0x0c -- Clear Screen
eAPR = 0x0d -- Active Position Return
eSO  = 0x0e -- Shift Out
eSI  = 0x0f -- Shift In
eCON = 0x11 -- Cursor on
eRPT = 0x12 -- RePeaT
eCOF = 0x14 -- Cursor off
eCAN = 0x18 -- CANcel
eSS2 = 0x19 -- Single Shift 2
eESC = 0x1b -- ESCape
eSS3 = 0x1d -- Single Shift 3
eAPH = 0x1e -- Active Position Home
eAPA = 0x1f -- Activate Position Address

-- * Serial supplementary control function set - ETS 300 072
eABK = 0x40 -- Alpha BlacK
eANR = 0x41 -- Alpha Red
eANG = 0x42 -- Alpha Green
eANY = 0x43 -- Alpha Yellow
eANB = 0x44 -- Alpha Blue
eANM = 0x45 -- Alpha Magenta
eANC = 0x46 -- Alpha Cyan
eANW = 0x47 -- Alpha White
eFSH = 0x48 -- FlaSH
eSTD = 0x49 -- STeaDy
eEBX = 0x4a -- End BoX
eSBX = 0x4b -- Start BoX
eNSZ = 0x4c -- Normal SiZe
eDBH = 0x4d -- DouBle Height
eDBW = 0x4e -- DouBle Width
eDBS = 0x4f -- DouBle Size
eMBK = 0x50 -- Mosaic BlacK
eMSR = 0x51 -- MoSaic Red
eMSG = 0x52 -- MoSaic Green
eMSY = 0x53 -- MoSaic Yellow
eMSB = 0x54 -- MoSaic Blue
eMSM = 0x55 -- MoSaic Magenta
eMSC = 0x56 -- MoSaic Cyan
eMSW = 0x57 -- MoSaic White
eCDY = 0x58 -- Conceal DisplaY
eSPL = 0x59 -- StoP Lining
eSTL = 0x5a -- StarT Lining
eCSI = 0x5b -- Control Sequence Introducer
eBBD = 0x5c -- Black BackgrounD
eNBD = 0x5d -- New BackgrounD
eHMS = 0x5e -- 
eRMS = 0x5f -- 

-- * Parallel supplementary control function set - ETS 300 072
eBKF = 0x40 -- BlacK Foreground
eRDF = 0x41 -- ReD Foreground
eGRF = 0x42 -- GReen Foreground
eYLF = 0x43 -- YeLlow Foreground
eBLF = 0x44 -- BLue Foreground
eMGF = 0x45 -- MaGenta Foreground
eCNF = 0x46 -- CyaN Foreground
eWHF = 0x47 -- WHite Foreground
eBKB = 0x50 -- BlacK Background
eRDB = 0x51 -- ReD Background
eGRB = 0x52 -- GReen Background
eYLB = 0x53 -- YeLlow Background
eBLB = 0x54 -- BLue Background
eMGB = 0x55 -- MaGenta Background
eCNB = 0x56 -- CyaN Background
eWHB = 0x57 -- WHite Background
eNPO = 0x5c -- Normal POlarity
eIPO = 0x5d -- Inverted POlarity
eTRB = 0x5e -- 
eSTC = 0x5f -- STop Conceal

--
sSSCFS = [eESC, 0x22, 0x40] -- Serial Supplementary Control Function Set
sPSCFS = [eESC, 0x22, 0x41] -- Parallel Supplementary Control Function Set

-- * Designation of Graphic Sets
sG0G0 = [eESC, 0x28, 0x40]
sG0G1 = [eESC, 0x29, 0x40]
sG0G2 = [eESC, 0x2a, 0x40]
sG0G3 = [eESC, 0x2b, 0x40]
sG'0G0 = [eESC, 0x28, 0x20, 0x42]

sG1G0 = [eESC, 0x28, 0x63]
sG1G1 = [eESC, 0x29, 0x63]
sG1G2 = [eESC, 0x2a, 0x63]
sG1G3 = [eESC, 0x2b, 0x63]
sG'1G1 = [eESC, 0x29, 0x20, 0x43]

sG2G0 = [eESC, 0x28, 0x62]
sG2G1 = [eESC, 0x29, 0x62]
sG2G2 = [eESC, 0x2a, 0x62]
sG2G3 = [eESC, 0x2b, 0x62]

sG3G0 = [eESC, 0x28, 0x64]
sG3G1 = [eESC, 0x29, 0x64]
sG3G2 = [eESC, 0x2a, 0x64]
sG3G3 = [eESC, 0x2b, 0x64]

sGGG0 = [eESC, 0x28, 0x21, 0x40]
sGGG1 = [eESC, 0x29, 0x21, 0x40]
sGGG2 = [eESC, 0x2a, 0x21, 0x40]
sGGG3 = [eESC, 0x2b, 0x21, 0x40]

-- * Protocol sequences
sPRO1 = [eESC, 0x39] -- PROtocol 1 (needs one more arguments)
sPRO2 = [eESC, 0x3a] -- PROtocol 2 (needs two more arguments)
sPRO3 = [eESC, 0x3b] -- PROtocol 3 (needs three more arguments)
sCSI  = [eESC, eCSI] -- Control Sequence Introducer

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
accCedilla     = [eSS2, 0x4b]
accGrave       = [eSS2, 0x41]
accAcute       = [eSS2, 0x42]
accCirconflexe = [eSS2, 0x43]
accUmlaut      = [eSS2, 0x48]

-- * Direction keys
keyUp          = [0x0b]
keyDown        = [0x0a]
keyLeft        = [0x08]
keyRight       = [0x09]

keyShiftUp    = sCSI ++ [0x4d]
keyShiftDown  = sCSI ++ [0x4c]
keyShiftLeft  = sCSI ++ [0x50]
keyShiftRight = sCSI ++ [0x34, 0x68]

ctrlLeft = [0x7f]

-- * Return key
keyReturn      = [0x0d]
keyShiftReturn = sCSI ++ [0x48]
keyCtrlReturn  = sCSI ++ [0x32, 0x4a]

-- * Function keys
keyFuncSend       = [aDC3, 0x41]
keyFuncPrev       = [aDC3, 0x42]
keyFuncRepeat     = [aDC3, 0x43]
keyFuncGuide      = [aDC3, 0x44]
keyFuncCancel     = [aDC3, 0x45]
keyFuncTOC        = [aDC3, 0x46]
keyFuncCorrection = [aDC3, 0x47]
keyFuncNext       = [aDC3, 0x48]
keyFuncConnection = [aDC3, 0x49]

