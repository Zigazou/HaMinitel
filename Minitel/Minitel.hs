-- Module Minitel
module Minitel.Minitel where

import Minitel.Generator
import Minitel.Queue
import Minitel.MString

import System.Hardware.Serialport
import qualified Data.ByteString as B

import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM

import Control.Monad

data Minitel = Minitel {
    serial   :: SerialPort,
    input    :: Queue,
    output   :: Queue,
    receiver :: ThreadId,
    sender   :: ThreadId
}

(<<<) :: Minitel -> MString -> IO ()
(<<<) minitel s = do
    putM (output minitel) s

mConfirmation :: Minitel -> MConfirmation -> IO Bool
mConfirmation minitel (mSend, mReceive) = do
    putM (output minitel) mSend
    answer <- readMString (get $ input minitel) completeReturn
    return (answer == mReceive)

mCall :: Minitel -> MCall -> IO MString
mCall minitel (mSend, count) = do
    putM (output minitel) mSend
    return =<< readCount (get $ input minitel) count

readCount :: (Eq a) => IO a -> Int -> IO [a]
readCount getter count = readMString getter isComplete
    where isComplete seq = length seq == count

readMString :: (Eq a) => IO a -> ([a] -> Bool) -> IO [a]
readMString getter isComplete = readMString' []
    where readMString' s
            | isComplete s = return s
            | s == []      = getter >>= \value -> readMString' [value]
            | otherwise    = do
                result <- waitFor 10000000 getter
                case result of
                    Just value -> readMString' $ s ++ [value]
                    Nothing    -> return s

waitFor :: (Eq a) => Int -> IO a -> IO (Maybe a)
waitFor delay getter = do
    done <- newEmptyMVar
    
    -- Run a race between the reader and the waiter
    reader <- forkIO $ getter            >>= \c -> putMVar done $ Just c
    waiter <- forkIO $ threadDelay delay >>        putMVar done Nothing

    -- Wait for the first to win
    result <- takeMVar done

    killThread reader
    killThread waiter

    return result

baseSettings = SerialPortSettings {
    commSpeed   = CS1200,
    bitsPerWord = 7,
    stopb       = One,
    parity      = Even,
    flowControl = NoFlowControl,
    timeout     = 1000000
    }

minitel :: String -> SerialPortSettings -> IO Minitel
minitel "" settings = minitel "/dev/ttyUSB0" settings
minitel dev settings = do
    port       <- openSerial dev settings
    sendQueue  <- atomically $ newTQueue
    recvQueue  <- atomically $ newTQueue
    sendThread <- forkIO $ sendLoop port sendQueue
    recvThread <- forkIO $ recvLoop port recvQueue

    return Minitel {
        serial    = port,
        input     = recvQueue,
        output    = sendQueue,
        receiver  = recvThread,
        sender    = sendThread
    }
  where recvLoop s q = forever $ do
            b <- recv s 1
            when (1 <= B.length b) $ (put q . fromIntegral . B.head) b
        sendLoop s q = forever $ get q >>= send s . B.singleton . fromIntegral

