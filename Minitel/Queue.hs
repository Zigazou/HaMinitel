-- Module Queue
module Minitel.Queue where

import Minitel.Sequence
import qualified Data.ByteString as B
import Control.Concurrent.MVar

type Queue = MVar SeqMinitel

put :: Queue -> SeqMinitel -> IO ()
put queue element = do
    tryTakeMVar queue >>= \a -> case a of
        Just v  -> putMVar queue $ v ++ element
        Nothing -> putMVar queue element

get :: Queue -> IO SeqMinitel
get queue = do
    tryTakeMVar queue >>= \a -> case a of
        Just v  -> return v
        Nothing -> return []

getW :: Queue -> IO SeqMinitel
getW = takeMVar
