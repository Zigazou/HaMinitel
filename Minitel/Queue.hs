-- Module Queue
module Minitel.Queue where

import Minitel.MString

import qualified Data.ByteString as B
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue, readTQueue)
import Control.Concurrent.STM (atomically)
import Control.Monad (forM_)

type Queue = TQueue Int

putM :: Queue -> MString -> IO ()
putM q s = forM_ s $ \ x -> do put q x

put :: Queue -> Int -> IO ()
put q = (atomically . writeTQueue q)

get :: Queue -> IO Int
get = atomically . readTQueue

