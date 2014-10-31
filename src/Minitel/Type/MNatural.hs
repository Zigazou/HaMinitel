{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Natural
Description : Natural numbers for HaMinitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Natural numbers for HaMinitel. The Minitel can only send and receive bytes
whose values are [0..127]. The MNat data type allows to use such numbers as if
they were standard Int while ensuring that we use only 7 bits.
-}

module Minitel.Type.MNatural (MNat, mnat, fromMNat) where

-- | The MNat type. The constructor is hidden.
newtype MNat = MakeMNat Int deriving (Real, Eq, Ord, Show)

-- | MNat is a bounded type
instance Bounded MNat where
    minBound = MakeMNat 0
    maxBound = MakeMNat 127

-- | Converts an Int into an MNat
mnat :: Int -> MNat
mnat x | loLimit <= x && x <= hiLimit = MakeMNat x
       | otherwise = error "Number out of bounds"
       where loLimit = fromIntegral (minBound :: MNat)
             hiLimit = fromIntegral (maxBound :: MNat)

-- | Converts an MNat into an Int
fromMNat :: MNat -> Int
fromMNat (MakeMNat i) = i

-- | Converts an Int binary function returning Int to a MNat binary function
--   returning an MNat
mfnat :: (Int -> Int) -> (MNat -> MNat)
mfnat f = mnat . f . fromMNat

mfnat2 :: (Int -> Int -> Int) -> (MNat -> MNat -> MNat)
mfnat2 f x y = mnat $ f (fromMNat x) (fromMNat y)

-- | You can do additions, substractions and multiplication with MNat
instance Num MNat where
    fromInteger = mnat . fromIntegral
    (+)         = mfnat2 (+)
    (-)         = mfnat2 (-)
    (*)         = mfnat2 (*)
    abs         = mfnat abs
    signum      = mfnat signum

-- | Allows to use toInteger with MNat
instance Integral MNat where
    quotRem x y = (fromInteger $ quot x' y', fromInteger $ rem x' y')
                  where (x', y') = (toInteger x, toInteger y)
    toInteger   = toInteger . fromMNat

-- | Allows to generate lists
instance Enum MNat where
    toEnum      = mnat
    fromEnum    = fromMNat

