-- Module Minitel
module Minitel.Minitel where

import Minitel.Generateur
import Minitel.Queue
import Minitel.Sequence
import System.Hardware.Serialport
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString as B

rcptBoucle :: SerialPort -> Queue -> IO ()
rcptBoucle s q = forever $ do
    b <- recv s 1
    when (1 <= B.length b) $ (put q . fromIntegral . B.head) b

emetBoucle :: SerialPort -> Queue -> IO ()
emetBoucle s q = forever $ get q >>= send s . B.singleton . fromIntegral

data Minitel = Minitel {
    serial    :: SerialPort,
    entree    :: Queue,
    sortie    :: Queue,
    recepteur :: ThreadId,
    emetteur  :: ThreadId
}

(<<<) :: Minitel -> SeqMinitel -> IO ()
(<<<) minitel seqEnvoi = do
    putSeq (sortie minitel) seqEnvoi

valideMinitel :: Minitel -> SeqValide -> IO Bool
valideMinitel minitel (seqEnvoi, seqReception) = do
    putSeq (sortie minitel) seqEnvoi
    reponse <- lireSequence (get $ entree minitel) seqComplete
    return (reponse == seqReception)

appelMinitel :: Minitel -> SeqAppel -> IO SeqMinitel
appelMinitel minitel (seqEnvoi, longueur) = do
    putSeq (sortie minitel) seqEnvoi
    return =<< lireFixe (get $ entree minitel) (fromIntegral longueur)

lireFixe :: (Eq a) => IO a -> Int -> IO [a]
lireFixe getter count = lireSequence getter isComplete
    where isComplete seq = length seq == count

lireSequence :: (Eq a) => IO a -> ([a] -> Bool) -> IO [a]
lireSequence getter isComplete = lireSequence' []
    where lireSequence' seq
            | isComplete seq = return seq
            | seq == []      = getter >>= \value -> lireSequence' [value]
            | otherwise      = do
                result <- waitFor 10000000 getter
                case result of
                    Just value -> lireSequence' $ seq ++ [value]
                    Nothing    -> return seq

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

minitelConfigurationStandard = SerialPortSettings {
    commSpeed = CS1200,
    bitsPerWord = 7,
    stopb = One,
    parity = Even,
    flowControl = NoFlowControl,
    timeout = 1000000
    }

minitel :: String -> SerialPortSettings -> IO Minitel
minitel "" settings = minitel "/dev/ttyUSB0" settings
minitel dev settings = do
    port <- openSerial dev settings
    emetQueue <- atomically $ newTQueue
    rcptQueue <- atomically $ newTQueue
    emet <- forkIO $ emetBoucle port emetQueue
    rcpt <- forkIO $ rcptBoucle port rcptQueue

    return Minitel {
        serial = port,
        entree = rcptQueue,
        sortie = emetQueue,
        recepteur = rcpt,
        emetteur = emet
    }
