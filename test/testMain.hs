{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : TestMain
Description : Tests for HaMinitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Tests for HaMinitel
-}
module Main where

import           Control.Monad       (unless)
import           System.Exit         (exitFailure)
import           Test.QuickCheck
import           Test.QuickCheck.All (quickCheckAll)

import           Minitel.Generate.Generator
import           Minitel.Generate.MDesign
import           Minitel.Type.Videotex

-- | Test mSize function
instance Arbitrary CharWidth where
    arbitrary = elements [SimpleWidth, DoubleWidth]

instance Arbitrary CharHeight where
    arbitrary = elements [SimpleHeight, DoubleHeight]

prop_msize l h = length (mSize l h) == 2

-- | Test mMove function
newtype SmallInt = SmallInt Int deriving (Show, Eq, Ord)
instance Arbitrary SmallInt where
    arbitrary = fmap SmallInt (choose (-127, 127))

prop_mmoveX (SmallInt x) = if x == 0 then length (mMove x 0) == 0
                                     else length (mMove x 0) >= 1

prop_mmoveY (SmallInt y) = if y == 0 then length (mMove 0 y) == 0
                                     else length (mMove 0 y) >= 1

-- | Test mDesign function
newtype StringList = StringList [String] deriving (Show, Eq)
instance Arbitrary StringList where
    arbitrary = do ls <- vectorOf 10 (vectorOf 8 (elements ['0', '1']))
                   return $ StringList ls

prop_mdesign1 (StringList design) =
    length (mDesign (charDesign design)) == 15

prop_mdesign2 (StringList design) =
    and $ map (\v -> v == 0x30 || v >= 0x40) (mDesign (charDesign design))

-- Helps TemplateHaskell work...
return []

main = do
    allPass <- $quickCheckAll
    unless allPass exitFailure
