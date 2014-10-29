{-|
Module      : Queue
Description : Shared queue for the Minitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Provides a shared queue for dealing with a Minitel
-}
module Minitel.Type.Queue where

import           Minitel.Type.MNatural
import           Minitel.Type.MString

import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TQueue (TQueue, readTQueue, writeTQueue)
import           Control.Monad                 (forM_)

-- | A queue is just a TQueue of MNat
type Queue = TQueue MNat

-- | Send an MString to the queue
putM :: Queue -> MString -> IO ()
putM q s = forM_ s $ \ x -> do put q x

-- | Send and Int to the queue
put :: Queue -> MNat -> IO ()
put q = (atomically . writeTQueue q)

-- | Blocking get to retrieve an Int from the queue
get :: Queue -> IO MNat
get = atomically . readTQueue

