{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : ASCII
Description : ASCII Control codes
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

ASCII control codes.
-}
module Minitel.Constants.ASCII where

import Minitel.Type.MNatural (MNat)

default (MNat)

-- * ASCII control codes
pattern NUL          = 0x00 -- Null
pattern SOH          = 0x01 -- Start Of Heading
pattern STX          = 0x02 -- Start of TeXt
pattern ETX          = 0x03 -- End of TeXt
pattern EOT          = 0x04 -- End Of Transmission
pattern ENQ          = 0x05 -- ENQuiry
pattern ACK          = 0x06 -- ACKnowledge
pattern BEL          = 0x07 -- BELl
pattern BS           = 0x08 -- BackSpace
pattern HT           = 0x09 -- Horizontal Tab
pattern LF           = 0x0a -- Line Feed / new line
pattern VT           = 0x0b -- Vertical Tab
pattern FF           = 0x0c -- Form Feed
pattern CR           = 0x0d -- Carriage Return
pattern SO           = 0x0e -- Shift Out
pattern SI           = 0x0f -- Shift In
pattern DLE          = 0x10 -- Data Link Escape
pattern DC1          = 0x11 -- Device Control 1
pattern DC2          = 0x12 -- Device Control 2
pattern DC3          = 0x13 -- Device Control 3
pattern DC4          = 0x14 -- Device Control 4
pattern NAK          = 0x15 -- Negative AcKnowledge
pattern SYN          = 0x16 -- SYNchronous idle
pattern ETB          = 0x17 -- End of Transmission Block
pattern CAN          = 0x18 -- CANcel
pattern EM           = 0x19 -- End of Medium
pattern SUB          = 0x1a -- SUBstitute
pattern ESC          = 0x1b -- ESCape
pattern FS           = 0x1c -- File Separator
pattern GS           = 0x1d -- Group Separator
pattern RS           = 0x1e -- Record Separator
pattern US           = 0x1f -- Unit Separator

-- * ASCII visible characters
pattern Space        = 0x20
pattern Exclamation  = 0x21
pattern DoubleQuotes = 0x22
pattern Hash         = 0x23
pattern Dollar       = 0x24
pattern Percent      = 0x25
pattern Ampersand    = 0x26
pattern Quote        = 0x27
pattern LeftParen    = 0x28
pattern RightParen   = 0x29
pattern Asterisk     = 0x2a
pattern Plus         = 0x2b
pattern Comma        = 0x2c
pattern Minus        = 0x2d
pattern Period       = 0x2e
pattern Slash        = 0x2f

pattern Digit0       = 0x30
pattern Digit1       = 0x31
pattern Digit2       = 0x32
pattern Digit3       = 0x33
pattern Digit4       = 0x34
pattern Digit5       = 0x35
pattern Digit6       = 0x36
pattern Digit7       = 0x37
pattern Digit8       = 0x38
pattern Digit9       = 0x39
pattern Colon        = 0x3a
pattern SemiColon    = 0x3b
pattern LessThan     = 0x3c
pattern Equal        = 0x3d
pattern GreaterThan  = 0x3e
pattern Question     = 0x3f

pattern At           = 0x40
pattern UpperA       = 0x41
pattern UpperB       = 0x42
pattern UpperC       = 0x43
pattern UpperD       = 0x44
pattern UpperE       = 0x45
pattern UpperF       = 0x46
pattern UpperG       = 0x47
pattern UpperH       = 0x48
pattern UpperI       = 0x49
pattern UpperJ       = 0x4a
pattern UpperK       = 0x4b
pattern UpperL       = 0x4c
pattern UpperM       = 0x4d
pattern UpperN       = 0x4e
pattern UpperO       = 0x4f
pattern UpperP       = 0x50
pattern UpperQ       = 0x51
pattern UpperR       = 0x52
pattern UpperS       = 0x53
pattern UpperT       = 0x54
pattern UpperU       = 0x55
pattern UpperV       = 0x56
pattern UpperW       = 0x57
pattern UpperX       = 0x58
pattern UpperY       = 0x59
pattern UpperZ       = 0x5a
pattern LeftSqBra    = 0x5b
pattern BackSlash    = 0x5c
pattern RightSqBra   = 0x5d
pattern Circumflex   = 0x5e
pattern UnderScore   = 0x5f

pattern BackTick     = 0x60
pattern LowerA       = 0x61
pattern LowerB       = 0x62
pattern LowerC       = 0x63
pattern LowerD       = 0x64
pattern LowerE       = 0x65
pattern LowerF       = 0x66
pattern LowerG       = 0x67
pattern LowerH       = 0x68
pattern LowerI       = 0x69
pattern LowerJ       = 0x6a
pattern LowerK       = 0x6b
pattern LowerL       = 0x6c
pattern LowerM       = 0x6d
pattern LowerN       = 0x6e
pattern LowerO       = 0x6f
pattern LowerP       = 0x70
pattern LowerQ       = 0x71
pattern LowerR       = 0x72
pattern LowerS       = 0x73
pattern LowerT       = 0x74
pattern LowerU       = 0x75
pattern LowerV       = 0x76
pattern LowerW       = 0x77
pattern LowerX       = 0x78
pattern LowerY       = 0x79
pattern LowerZ       = 0x7a
pattern LeftCuBra    = 0x7b
pattern Pipe         = 0x7c
pattern RightCuBra   = 0x7d
pattern Tilde        = 0x7e
pattern Del          = 0x7f

