-- Module Queue
module Minitel.Queue where

import Minitel.Sequence
import qualified Data.ByteString as B
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Control.Monad

type Queue = TQueue Integer

putSeq :: Queue -> SeqMinitel -> IO ()
putSeq q s = forM_ s $ \ x -> do put q x

put :: Queue -> Integer -> IO ()
put q v = (atomically . writeTQueue q) v

get :: Queue -> IO Integer
get q = (atomically . readTQueue) q

