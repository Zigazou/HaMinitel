{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Minitel
Description : Interface to the Minitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module provides to deal with Minitel communications.
-}
module Minitel.Minitel where

import           Minitel.Constants
import           Minitel.Generator
import           Minitel.MNatural
import           Minitel.MString
import           Minitel.Queue

import qualified Data.ByteString               as B
import           System.Hardware.Serialport

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue

import           Control.Monad

import           Data.Char

class Sendable a where
    (<<<) :: Minitel -> a -> IO ()

-- | Structure to hold Minitel components
data Minitel = Minitel
    { serial   :: SerialPort -- ^ Serial port to which the Minitel is connected
    , input    :: Queue      -- ^ What we receive from the Minitel
    , output   :: Queue      -- ^ What we send to the Minitel
    , receiver :: ThreadId   -- ^ Receiver thread, allowing full-duplex
    , sender   :: ThreadId   -- ^ Sender thread, allowing full-duplex
    }

-- | Operator to send an MString to the Minitel
instance Sendable MString where
    (<<<) minitel = putM (output minitel)

instance Sendable [MString] where
    (<<<) minitel = mapM_ (minitel <<<)

instance Sendable (Maybe [MString]) where
    (<<<) minitel (Just ms) = mapM_ (minitel <<<) ms
    (<<<) minitel Nothing   = return ()

instance Sendable Char where
    (<<<) minitel c = putM (output minitel) [(toMNat . ord) c]

instance Sendable MCall where
    (<<<) minitel (mSend, _) = minitel <<< mSend

instance Sendable [MCall] where
    (<<<) minitel = mapM_ (mCall minitel)

-- | Sends an MString to the Minitel and waits for its answer. The answer
--   should be the awaited one specified in the MConfirmation. If there is
--   no answer from the Minitel or the answer is not the right one, returns
--   False
mConfirmation :: Minitel -> MConfirmation -> IO Bool
mConfirmation minitel (mSend, mReceive) = do
    putM (output minitel) mSend
    answer <- readNMString (getter minitel) completeReturn
    return (answer == mReceive)

-- | Sends an MString to the Minitel and waits for its answer. It returns
--   an MString of max length as specified in the MCall.
mCall :: Minitel -> MCall -> IO MString
mCall minitel (mSend, count) = do
    putM (output minitel) mSend
    return =<< readNCount (getter minitel) (fromMNat count)

-- | Waits for an MString of @count@ elements coming from the Minitel. If it
--   takes too long, returns what has already been collected.
--   This is the blocking version.
readBCount :: (Eq a) => IO a -> Int -> IO [a]
readBCount getter count = readBMString getter isComplete
    where isComplete seq = length seq == count

-- | Waits for an MString of @count@ elements coming from the Minitel. If it
--   takes too long, returns what has already been collected.
--   This is the non-blocking version
readNCount :: (Eq a) => IO a -> Int -> IO [a]
readNCount getter count = readNMString getter isComplete
    where isComplete seq = length seq == count

-- | Returns the getter for a Minitel
getter :: Minitel -> IO MNat
getter minitel = get $ input minitel

-- | Waits for a complete MString coming from the Minitel. If it takes too
--   long, returns what has already been collected. To determine if the MString
--   is complete, it needs an @isComplete@ function which tells if an MString
--   is complete (True) or not (False).
--   This is the blocking version.
readBMString :: (Eq a) => IO a -> ([a] -> Bool) -> IO [a]
readBMString getter isComplete = readBMString' []
    where readBMString' s
            | isComplete s = return s
            | s == []      = getter >>= \value -> readBMString' [value]
            | otherwise    = do
                result <- waitFor 1000000 getter
                case result of
                    Just value -> readBMString' $ s ++ [value]
                    Nothing    -> return s

-- | Waits for a complete MString coming from the Minitel. If it takes too
--   long, returns what has already been collected. To determine if the MString
--   is complete, it needs an @isComplete@ function which tells if an MString
--   is complete (True) or not (False).
--   This is the non-blocking version
readNMString :: (Eq a) => IO a -> ([a] -> Bool) -> IO [a]
readNMString getter isComplete = readNMString' []
    where readNMString' s
            | isComplete s = return s
            | otherwise    = do
                result <- waitFor 200000 getter
                case result of
                    Just value -> readNMString' $ s ++ [value]
                    Nothing    -> return s

-- | Waits for either a read to succeed or a delay to end. It does this by
--   running two threads. The first one to press the buzzer will stop the
--   function and returns the result (either Just a or Nothing)
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

-- | Base settings for the serial port on which is connected a Minitel.
--   Standard configuration is 1200 bps, 7 bits, 1 stop, even parity.
baseSettings = SerialPortSettings
    { commSpeed   = CS1200
    , bitsPerWord = 7
    , stopb       = One
    , parity      = Even
    , flowControl = NoFlowControl
    , timeout     = 10
    }

-- | Translates bit rate to SerialPort types
spBitRate :: Int -> CommSpeed
spBitRate 300  = CS300
spBitRate 1200 = CS1200
spBitRate 4800 = CS4800
spBitRate 9600 = CS9600
spBitRate _    = error "Unsupported bit rate"

-- | Change Minitel speed
setSpeed :: Minitel -> Int -> IO Minitel
setSpeed m rate = do
    waitForMinitel m
    m <<< mSpeed rate
    threadDelay 500000
    killMinitel m
    let settings = baseSettings { commSpeed = spBitRate rate }
    m <- minitel "" settings
    return m

    --answer <- mCall m $ mSpeed rate
    {-
    if length answer == pro2Length
        then return m
        else do
            killMinitel m
            let settings = baseSettings { commSpeed = spBitRate rate }
            m <- minitel "" settings
            return m
    -}

-- | waitForMinitel waits till the Minitel has displayed everything
waitForMinitel :: Minitel -> IO MString
waitForMinitel minitel = blockOn mIdentification
    where blockOn (mSend, count) = do
            putM (output minitel) mSend
            return =<< readBCount (getter minitel) (fromMNat count)

-- | Opens a full-duplex connection to a Minitel. The default serial is set
--   to \/dev\/ttyUSB0.
minitel :: String -> SerialPortSettings -> IO Minitel
minitel "" settings = minitel "/dev/ttyUSB0" settings
minitel dev settings = do
    port       <- openSerial dev settings
    sendQueue  <- atomically $ newTQueue
    recvQueue  <- atomically $ newTQueue
    sendThread <- forkIO $ sendLoop port sendQueue
    recvThread <- forkIO $ recvLoop port recvQueue

    return Minitel
        { serial    = port
        , input     = recvQueue
        , output    = sendQueue
        , receiver  = recvThread
        , sender    = sendThread
        }
  where sendLoop s q = forever $ get q >>= send s . B.singleton . fromIntegral
        recvLoop s q = forever $ do
            b <- recv s 1
            when (1 <= B.length b) $ (put q . fromIntegral . B.head) b

-- | Kills a minitel, stopping its threads
killMinitel :: Minitel -> IO ()
killMinitel minitel = do
    killThread $ sender minitel
    killThread $ receiver minitel

