module Minitel.Constants where

import Data.Char

default (Int, Double)

-- ASCII control codes
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

-- Protocol sequences
pro1 = [esc, 0x39] -- protocol 1 (needs one more arguments)
pro2 = [esc, 0x3a] -- protocol 2 (needs two more arguments)
pro3 = [esc, 0x3b] -- protocol 3 (needs three more arguments)
csi  = [esc, 0x5b] -- CSI

-- PRO1 commands
deconnexion = 0x67
connexion = 0x68
ret1 = 0x6c
ret2 = 0x6d
oppo = 0x6f
statusTerminal = 0x70
statusClavier = 0x72
statusFonctionnement = 0x72
statusVitesse = 0x74
statusProtocole = 0x76
enqrom = 0x7b
reset = 0x7f

-- PRO2 commands
copie = 0x7c
aiguillageTo = 0x62
nonDiffusion = 0x64
nonRetourAcquittement = 0x64
diffusion = 0x65
retourAcquittement = 0x65
transparence = 0x66
start = 0x69
stop = 0x6a
prog = 0x6b
repStatusClavier = 0x73
repStatusFonctionnement = 0x73
repStatusProtocole = 0x77
telinfo = [0x31, 0x7d]
mixed1 = [0x32, 0x7d]
mixed2 = [0x32, 0x7e]

-- PRO3 Commands
switchOff = 0x60
switchOn = 0x61
switchFrom = 0x63

-- Longueurs commandes PRO
pro1Length = 3
pro2Length = 4
pro3Length = 5

-- Autres codes
copyFrench = 0x6a
copyAmerican = 0x6b
eten = 0x41
c0 = 0x43

-- PRO2+START/STOP codes
rollMode = 0x43
procedure = 0x44
lowercase = 0x45

-- PRO2+PROG codes
b9600 = 0x7f
b4800 = 0x76
b1200 = 0x64
b300 = 0x52

-- PRO3+START/STOP codes
-- eten = 0x41
-- c0 = 0x43

-- Reception codes
rcptScreen = 0x58
rcptKeyboard = 0x59
rcptModem = 0x5a
rcptPlug = 0x5b
rcptPhone = 0x5c
rcptSoftware = 0x5d

-- Emission codes
emitScreen = 0x50
emitKeyboard = 0x51
emitModem = 0x52
emitPlug = 0x53
emitPhone = 0x54
emitSoftware = 0x55

-- Accents
accentCedilla     = [ss2, 0x4b]
accentGrave       = [ss2, 0x41]
accentAcute       = [ss2, 0x42]
accentCirconflexe = [ss2, 0x43]
accentUmlaut      = [ss2, 0x48]

-- Direction keys
keyUp    = csi ++ [0x41]
keyDown  = csi ++ [0x42]
keyLeft  = csi ++ [0x44]
keyRight = csi ++ [0x43]

keyShiftUp    = csi ++ [0x4d]
keyShiftDown  = csi ++ [0x4c]
keyShiftLeft  = csi ++ [0x50]
keyShiftRight = csi ++ [0x34, 0x68]

ctrlLeft = 0x7f

-- Return key
keyReturn      = 0x0d
keyShiftReturn = csi ++ [0x48]
keyCtrlReturn  = csi ++ [0x32, 0x4a]

-- Touches de fonction
keyFuncSend       = [dc3, 0x41]
keyFuncReturn     = [dc3, 0x42]
keyFuncRepeat     = [dc3, 0x43]
keyFuncGuide      = [dc3, 0x44]
keyFuncCancel     = [dc3, 0x45]
keyFuncTOC        = [dc3, 0x46]
keyFuncCorrection = [dc3, 0x47]
keyFuncNext       = [dc3, 0x48]
keyFuncConnection = [dc3, 0x49]

-- Minitel ability
data Ability = Ability {
    id         :: Char,
    name       :: String,
    reversible :: Bool,
    keyboard   :: String,
    maxSpeed   :: Int,
    cols80     :: Bool,
    charDefine :: Bool
} deriving (Show)

minitelAbilities = [
    Ability 'c' "Minitel 1"         False "ABCD"   1200 False False,
    Ability 'd' "Minitel 10"        False "Azerty" 1200 False False,
    Ability 'e' "Minitel 1 couleur" False "Azerty" 1200 False False,
    Ability 'f' "Minitel 10"        True  "Azerty" 1200 False False,
    Ability 'g' "Émulateur"         True  "Azerty" 9600 True  True,
    Ability 'j' "Imprimante"        False ""       1200 False False,
    Ability 'r' "Minitel 1"         True  "Azerty" 1200 False False,
    Ability 's' "Minitel 1 couleur" True  "Azerty" 1200 False False,
    Ability 't' "Terminatel 252"    False ""       1200 False False,
    Ability 'u' "Minitel 1B"        True  "Azerty" 4800 True  False,
    Ability 'v' "Minitel 2"         True  "Azerty" 9600 True  True,
    Ability 'w' "Minitel 10B"       True  "Azerty" 4800 True  False,
    Ability 'y' "Minitel 5"         True  "Azerty" 9600 True  True,
    Ability 'z' "Minitel 12"        True  "Azerty" 9600 True  True
    ]

-- Base abilities
baseAbilities =
    Ability '*' "Minitel inconnu"   False "ABCD"   1200 False False

-- Maker identification codes
makerCodes = [
    ('A', "Matra"),
    ('B', "RTIC"),
    ('C', "Telic-Alcatel"),
    ('D', "Thomson"),
    ('E', "CCS"),
    ('F', "Fiet"),
    ('G', "Fime"),
    ('H', "Unitel"),
    ('I', "Option"),
    ('J', "Bull"),
    ('K', "Télématique"),
    ('L', "Desmet")
    ]

