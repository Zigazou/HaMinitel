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

import           Minitel.Generator

instance Arbitrary CharWidth where
    arbitrary = elements [SimpleWidth, DoubleWidth]

instance Arbitrary CharHeight where
    arbitrary = elements [SimpleHeight, DoubleHeight]

prop_msize l h = length (mSize l h) == 2

-- Helps TemplateHaskell work...
return []

main = do
    allPass <- $quickCheckAll
    unless allPass exitFailure
