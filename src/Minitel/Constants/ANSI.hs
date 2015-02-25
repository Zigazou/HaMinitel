{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : ANSI
Description : ANSI escape codes
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

ANSI escape codes.
-}
module Minitel.Constants.ANSI where

import Minitel.Constants.ASCII as ASCII

-- * Column number 04
pattern ICH  = ASCII.At           -- Insert CHaracter
pattern CUU  = ASCII.UpperA       -- CUrsor Up
pattern CUD  = ASCII.UpperB       -- CUrsor Down
pattern CUF  = ASCII.UpperC       -- CUrsor Forward
pattern CUB  = ASCII.UpperD       -- CUrsor Back
pattern CNL  = ASCII.UpperE       -- Cursor Next Line
pattern CPL  = ASCII.UpperF       -- Cursor Preceding Line
pattern CHA  = ASCII.UpperG       -- Cursor Horizontal Absolute
pattern CUP  = ASCII.UpperH       -- CUrsor Position
pattern CHT  = ASCII.UpperI       -- Cursor Horizontal Tabulation
pattern ED   = ASCII.UpperJ       -- Erase Display
pattern EL   = ASCII.UpperK       -- Erase in Line
pattern IL   = ASCII.UpperL       -- Insert Line
pattern DL   = ASCII.UpperM       -- Delete Line
pattern EF   = ASCII.UpperN       -- Erase in Field
pattern EA   = ASCII.UpperO       -- Erase in Area

-- * Column number 05
pattern DCH  = ASCII.UpperP       -- Delete CHaracter
pattern SEE  = ASCII.UpperQ       -- Select Editing Extent
pattern CPR  = ASCII.UpperR       -- Cursor Position Report
pattern SU   = ASCII.UpperS       -- Scroll Up
pattern SD   = ASCII.UpperT       -- Scroll Down
pattern NP   = ASCII.UpperU       -- Next Page
pattern PP   = ASCII.UpperV       -- Preceding Page
pattern CTC  = ASCII.UpperW       -- Cursor Tabulation Control
pattern ECH  = ASCII.UpperX       -- Erase CHaracter
pattern CVT  = ASCII.UpperY       -- Cursor Vertical Tabulation
pattern CBT  = ASCII.UpperZ       -- Cursor Backward Tabulation
pattern SRS  = ASCII.LeftSqBra    -- Start Reversed String
pattern PTX  = ASCII.BackSlash    -- Parallel TeXts
pattern SDS  = ASCII.RightSqBra   -- Start Directed String
pattern SIMD = ASCII.Circumflex   -- Select Implicit Movmement Direction
--                                -- ASCII.UnderScore not used

-- * Column number 06
pattern HPA  = ASCII.BackTick     -- Horizontal Position Absolute
pattern HPR  = ASCII.LowerA       -- Horizontal Position Relative
pattern REP  = ASCII.LowerB       -- REPeat
pattern DA   = ASCII.LowerC       -- Device Attributes
pattern VPA  = ASCII.LowerD       -- Vertical Position Absolute
pattern VPR  = ASCII.LowerE       -- Vertical Position Relative
pattern HVP  = ASCII.LowerF       -- Horizontal and Vertical Position
pattern TBC  = ASCII.LowerG       -- TaBulation Clear
pattern SM   = ASCII.LowerH       -- Set Mode
pattern MC   = ASCII.LowerI       -- Media Copy
pattern HPB  = ASCII.LowerJ       -- Horizontal Position Backward
pattern VPB  = ASCII.LowerK       -- Vertical Position Backward
pattern RM   = ASCII.LowerL       -- Reset Mode
pattern SGR  = ASCII.LowerM       -- Select Graphic Rendition
pattern DSR  = ASCII.LowerN       -- Device Status Report
pattern DAQ  = ASCII.LowerO       -- Define Area Qualification

-- * Column number 07
--pattern SCP  = ASCII.LowerS       -- Save Cursor Position
--pattern RCP  = ASCII.LowerU       -- Restore Cursor Position

-- * EL and ED parameter values
pattern CursorToEnd       = 0
pattern BeginningToCursor = 1
pattern AllCharacters     = 2

-- * SM (Set Mode) parameter values
pattern GATM = 1  -- Guarded Area Transfer Mode
pattern KAM  = 2  -- Keyboard Action Mode
pattern CRM  = 3  -- Control Representation Mode
pattern IRM  = 4  -- Insertion Replacement Mode
pattern SRTM = 5  -- Status Report Transfer Mode
pattern ERM  = 6  -- ERasure Mode
pattern VEM  = 7  -- Vertical Editing Mode
pattern BDSM = 8  -- Bi-Directional Support Mode
pattern DCSM = 9  -- Device Component Select Mode
pattern HEM  = 10 -- Horizontal Editing Mode
pattern PUM  = 11 -- Positioning Unit Mode
pattern SRM  = 12 -- Send/Receive mode
pattern FEAM = 13 -- Format Effector Action Mode
pattern FETM = 14 -- Format Effector Transfer Mode
pattern MATM = 15 -- Multiple Area Transfer Mode
pattern TTM  = 16 -- Transfer Termination Mode
pattern SATM = 17 -- Selected Area Transfer Mode
pattern TSM  = 18 -- Tabulation Stop Mode
pattern GRCM = 21 -- Grpahic Rendition CoMbination
pattern ZDM  = 22 -- Zero Default Mode

