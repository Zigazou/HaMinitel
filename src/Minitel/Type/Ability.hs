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
    { id         :: Char   -- ^ A Minitel is identified by a single letter
    , name       :: String -- ^ Human french name
    , reversible :: Bool   -- ^ Some Minitels can be used as a standard modem
    , keyboard   :: String -- ^ First Minitels were ABCD, then Azerty or none
    , maxSpeed   :: Int    -- ^ Max supported speed in bauds
    , cols80     :: Bool   -- ^ Is terminal mode (80 columns) supported ?
    , charDefine :: Bool   -- ^ are characters redefinable ?
    } deriving (Show)
