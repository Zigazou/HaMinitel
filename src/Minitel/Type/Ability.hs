{-|
Module      : Ability
Description : 
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

-}
module Minitel.Type.Ability where

-- | Minitel ability describes what the Minitel is able to do, since the
--   Minitel is a generic name for a complete family of terminals
data Ability = Ability
    { abilityId         :: Char   -- ^ A Minitel is identified by a single
                                  --   letter
    , abilityName       :: String -- ^ Human french name
    , abilityReversible :: Bool   -- ^ Some Minitels can be used as a standard
                                  --   modem
    , abilityKeyboard   :: String -- ^ First Minitels were ABCD, then Azerty or
                                  --   none
    , abilityMaxSpeed   :: Int    -- ^ Max supported speed in bauds
    , abilityCols80     :: Bool   -- ^ Is terminal mode (80 columns) supported ?
    , abilityCharDefine :: Bool   -- ^ are characters redefinable ?
    , abilityPhoto      :: Bool   -- ^ Is JPEG display supported ?
    } deriving (Show)

data Maker = Maker
    { makerId   :: Char    -- ^ A Maker is identified by a single letter
    , makerName :: String  -- ^ Human french maker name
    } deriving (Show)

