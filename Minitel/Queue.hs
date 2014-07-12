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
module Minitel.Queue where

import Minitel.MString

import qualified Data.ByteString as B
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue, readTQueue)
import Control.Concurrent.STM (atomically)
import Control.Monad (forM_)

-- | A queue is just a TQueue of Int
type Queue = TQueue Int

-- | Send an MString to the queue
putM :: Queue -> MString -> IO ()
putM q s = forM_ s $ \ x -> do put q x

-- | Send and Int to the queue
put :: Queue -> Int -> IO ()
put q = (atomically . writeTQueue q)

-- | Blocking get to retrieve an Int from the queue
get :: Queue -> IO Int
get = atomically . readTQueue

