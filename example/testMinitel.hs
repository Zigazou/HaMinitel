import Minitel.Minitel
import Minitel.Generateur
import Minitel.Queue
import Minitel.Sequence
import Control.Concurrent
import Control.Monad
import System.Hardware.Serialport

haskellMinitel :: Minitel -> IO ()
haskellMinitel m = forM_ [1 .. 20] $ \ x -> do
    m <<< seqPosition x x
    m <<< seqString VideoTex "Haskell Minitel!"

main = do
    m <- minitel "/dev/ttyUSB0" minitelConfigurationStandard

    mapM_ (appelMinitel m) [
        seqClavierEtendu True,
        seqClavierCurseur True,
        seqClavierMinuscule True
        ]

    mapM_ (m <<<) [
        seqEfface EffVraimentTout,
        seqPosition 1 0,
        seqString VideoTex "Démonstration de HaMinitel"
        ]

    haskellMinitel m

    forever $ do
        s <- lireSequence (get $ entree m) seqComplete
        putStrLn $ show s

    flush (serial m)
