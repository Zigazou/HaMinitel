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
newtype MNat = MakeMNat Int

-- | MNat is a bounded type
instance Bounded MNat where
    minBound = MakeMNat 0
    maxBound = MakeMNat 127

-- | Converts an Int into an MNat
mnat :: Int -> MNat
mnat x | fromIntegral (minBound :: MNat) <= x && x <= fromIntegral (maxBound :: MNat) = MakeMNat x
       | otherwise = error "Number out of bounds"

-- | Converts an MNat into an Int
fromMNat :: MNat -> Int
fromMNat (MakeMNat i) = i

-- | Converts an Int binary function to a MNat binary function
fnat :: (Int -> Int -> t) -> (MNat -> MNat -> t)
fnat f x y = f (fromMNat x) (fromMNat y)

-- | Converts an Int binary function returning Int to a MNat binary function
--   returning an MNat
mfnat :: (Int -> Int -> Int) -> (MNat -> MNat -> MNat)
mfnat f x y = mnat $ fnat f x y

-- | You can do additions, substractions and multiplication with MNat
instance Num MNat where
    fromInteger = mnat . fromIntegral
    (+)         = mfnat (+)
    (-)         = mfnat (-)
    (*)         = mfnat (*)
    abs x       = x
    signum      = mnat . signum . fromMNat

-- | Allows to use toInteger with MNat
instance Integral MNat where
    quotRem x y = (fromInteger $ x' `quot` y', fromInteger $ x' `rem` y')
                  where (x', y') = (toInteger x, toInteger y)
    toInteger   = toInteger . fromMNat

-- | Allows to generate lists
instance Enum MNat where
    toEnum      = mnat
    fromEnum    = fromMNat

-- | MNat can do Real, Ord, Eq and Show
instance Real MNat where toRational  = fromIntegral . fromMNat
instance Ord  MNat where (<=)        = fnat (<=)
instance Eq   MNat where (==)        = fnat (==)
instance Show MNat where show        = show . fromMNat
