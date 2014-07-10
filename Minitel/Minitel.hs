-- Module Minitel
module Minitel.Minitel where

import Minitel.Generateur
import Minitel.Queue
import Minitel.Sequence
import System.Hardware.Serialport
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

recepteurBoucle :: SerialPort -> Queue -> IO ()
recepteurBoucle s q = forever $ recv s 10 >>= put q . seqMinitel

emetteurBoucle :: SerialPort -> Queue -> IO ()
emetteurBoucle s q = forever $ get q >>= send s . seqByteString

data Minitel = Minitel {
    serial    :: SerialPort,
    entree    :: Queue,
    sortie    :: Queue,
    recepteur :: ThreadId,
    emetteur  :: ThreadId
}

(<<<) :: Minitel -> SeqMinitel -> IO ()
(<<<) minitel seqEnvoi = do
    put (sortie minitel) seqEnvoi

valideMinitel :: Minitel -> SeqValide -> IO Bool
valideMinitel minitel (seqEnvoi, seqReception) = do
    put (sortie minitel) seqEnvoi
    reponse <- get $ entree minitel
    return (reponse == seqReception)

appelMinitel :: Minitel -> SeqAppel -> IO SeqMinitel
appelMinitel minitel (seqEnvoi, longueur) = do
    put (sortie minitel) seqEnvoi
    reponse <- get $ entree minitel
    return reponse

minitelConfigurationStandard = SerialPortSettings {
    commSpeed = CS1200,
    bitsPerWord = 7,
    stopb = One,
    parity = Even,
    flowControl = NoFlowControl,
    timeout = 1
    }

minitel :: String -> SerialPortSettings -> IO Minitel
minitel "" settings = minitel "/dev/ttyUSB0" settings
minitel port settings = do
    emetteurQueue <- newEmptyMVar
    recepteurQueue <- newEmptyMVar
    s <- openSerial port settings
    r <- forkIO $ recepteurBoucle s recepteurQueue
    e <- forkIO $ emetteurBoucle s emetteurQueue

    return Minitel {
        serial = s,
        entree = recepteurQueue,
        sortie = emetteurQueue,
        recepteur = r,
        emetteur = e
    }
