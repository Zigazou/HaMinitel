{-|
Module      : Signal
Description : Signal model
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module provides a signal management designed for the Minitel.
-}
module Minitel.UI.Signal where

import Minitel.Key

data Signal = Key
            | EnterWidget
            | LeaveWidget

