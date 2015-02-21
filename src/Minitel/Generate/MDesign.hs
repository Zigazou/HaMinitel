{-|
Module      : MDesign
Description : MDesign generator
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module allows to redefine characters (Minitel 2).
-}
module Minitel.Generate.MDesign (
    CharDesign, charDesign, mDefineSet, mDesign, mRedesign
) where

import Data.List.Split (chunksOf)
import Minitel.Constants.Constants
import Minitel.Type.MNatural (MNat)
import Minitel.Type.MString (MString)
import Minitel.Type.Videotex (CharSet (G'0, G'1)) 
import Minitel.Generate.Generator (mUseSet)

default (MNat)

-- | A character design is a list of 10 Strings. Each string contains 8
--   characters, either '1' or '0', thus allowing you to simple write the
--   character design using Haskell code.
data CharDesign = MakeCharDesign [String]

charDesign :: [String] -> CharDesign
charDesign design
    | length design /= 10               = error "Wrong number of strings"
    | all (\ i -> length i == 8) design = error "Wrong string size"
    | otherwise                         = MakeCharDesign design

-- | Chooses which character set will be redefined. Useful only for the
--   mRedesign function.
mDefineSet :: CharSet -> MString
mDefineSet G'0 = [aUS, 0x23, 0x20, 0x20, 0x20, 0x42, 0x49]
mDefineSet G'1 = [aUS, 0x23, 0x20, 0x20, 0x20, 0x43, 0x49]
mDefineSet _   = error "G0 or G1 charsets cannot be redefined"

-- | Generate the MString used to redefine one character. Useful only for the
--   mRedesign function
mDesign :: CharDesign -> MString
mDesign (MakeCharDesign design) =
    0x30 : map bitsToMtel ((chunksOf 6 . concat) design)
    where bitsToNat = foldl (\a v -> 2 * a + (if v == '1' then 1 else 0)) 0
          bitsToMtel s = 0x40 + bitsToNat s * (if length s == 2 then 16 else 1)

-- | Generate the MString used to redefine several characters. Useful only for
--   the mRedesign function
mDesigns :: [CharDesign] -> [MString]
mDesigns = map mDesign

-- | Redefine characters given the ord of the first character as the first
--   argument. The character set must also be indicated. It will be
--   automatically selected after the redefinition.
mRedesign :: MNat -> [CharDesign] -> CharSet -> MString
mRedesign fromChar designs charset =
    mDefineSet charset
    ++ [aUS, 0x23, fromChar]
    ++ (concat . mDesigns) designs
    ++ [aUS, 0x41, 0x41]
    ++ mUseSet charset
