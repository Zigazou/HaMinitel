{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Natural
Description : Natural numbers for HaMinitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Natural numbers for HaMinitel. The Minitel can only send and receive bytes
whose values are [0..127]. The MNatN data type allows to use such numbers as if
they were standard Int while ensuring that we use only 7 bits.
-}

module Minitel.Type.MNatural (MNat, MNatN, mnat, fromMNat) where

import GHC.TypeLits
import Data.Proxy

-- | The MNatN type. The constructor is hidden.
newtype MNatN (lo :: Nat) (hi :: Nat) = MakeMNat Int
    deriving (Real, Eq, Ord, Show)

type MNat = MNatN 0 127

-- | MNatN is a bounded type
instance (KnownNat lo, KnownNat hi) => Bounded (MNatN lo hi) where
    minBound = MakeMNat . fromInteger $ natVal (Proxy :: Proxy lo)
    maxBound = MakeMNat . fromInteger $ natVal (Proxy :: Proxy hi)

-- | Converts an MNatN into an Int
fromMNat :: (KnownNat lo, KnownNat hi) => MNatN lo hi -> Int
fromMNat (MakeMNat i) = i

-- | Converts an Int into an MNatN
mnat :: (KnownNat lo, KnownNat hi) => Int -> MNatN lo hi
mnat = mnat' minBound maxBound
    where mnat' :: (KnownNat lo, KnownNat hi) =>
                   MNatN lo hi -> MNatN lo hi -> Int -> MNatN lo hi
          mnat' loB hiB x | fromMNat loB <= x && x <= fromMNat hiB = MakeMNat x
                          | otherwise = error "Number out of bounds"

-- | Converts an Int binary function returning Int to a MNatN binary function
--   returning an MNatN
mfnat :: (KnownNat lo, KnownNat hi) =>
         (Int -> Int) -> (MNatN lo hi -> MNatN lo hi)
mfnat f = mnat . f . fromMNat

mfnat2 :: (KnownNat lo, KnownNat hi) =>
          (Int -> Int -> Int) -> (MNatN lo hi -> MNatN lo hi -> MNatN lo hi)
mfnat2 f x y = mnat $ f (fromMNat x) (fromMNat y)

-- | You can do additions, substractions and multiplication with MNatN
instance (KnownNat lo, KnownNat hi) => Num (MNatN lo hi) where
    fromInteger = mnat . fromIntegral
    (+)         = mfnat2 (+)
    (-)         = mfnat2 (-)
    (*)         = mfnat2 (*)
    abs         = mfnat abs
    signum      = mfnat signum

-- | Allows to use toInteger with MNatN
instance (KnownNat lo, KnownNat hi) => Integral (MNatN lo hi) where
    quotRem x y = (fromInteger $ quot x' y', fromInteger $ rem x' y')
                  where (x', y') = (toInteger x, toInteger y)
    toInteger   = toInteger . fromMNat

-- | Allows to generate lists
instance (KnownNat lo, KnownNat hi) => Enum (MNatN lo hi) where
    toEnum      = mnat
    fromEnum    = fromMNat
