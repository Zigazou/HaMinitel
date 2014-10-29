{-|
Module      : Natural
Description : Natural numbers for HaMinitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Natural numbers for HaMinitel
-}

module Minitel.MNatural where

newtype MNat = MakeMNat Int

toMNat :: Int -> MNat
toMNat x | x < 0     = error "Can't create negative naturals!"
             | x > 127   = error "Can't create naturals above 127!"
             | otherwise = MakeMNat x

fromMNat :: MNat -> Int
fromMNat (MakeMNat i) = i

instance Bounded MNat where
    minBound = MakeMNat 0
    maxBound = MakeMNat 127

instance Num MNat where
    fromInteger = toMNat . fromIntegral
    x + y       = toMNat (fromMNat x + fromMNat y)
    x - y       = toMNat (fromMNat x - fromMNat y)
    x * y       = toMNat (fromMNat x * fromMNat y)
    abs x       = x
    signum x    = if fromMNat x == 0 then 0 else 1

instance Real MNat where
    toRational  = fromIntegral . fromMNat

instance Integral MNat where
    quotRem x y = (fromInteger $ x' `quot` y', fromInteger $ x' `rem` y')
                where x' = toInteger x
                      y' = toInteger y
    toInteger   = toInteger . fromMNat

instance Enum MNat where
    toEnum      = toMNat
    fromEnum    = fromMNat

instance Ord MNat where
    x <= y      = fromMNat x <= fromMNat y

instance Eq MNat where
    (==) x y    = fromMNat x == fromMNat y

instance Show MNat where
    show        = show . fromMNat
