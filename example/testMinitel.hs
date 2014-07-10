import Minitel.Minitel
import Minitel.Generateur
import Minitel.Queue
import Control.Concurrent
import Control.Monad
import System.Hardware.Serialport

haskellMinitel :: Minitel -> IO ()
haskellMinitel m = forM_ [1..20] $ \x -> do
    m <<< seqPosition x x
    m <<< seqString VideoTex "Haskell Minitel!"

main = do
    m <- minitel "/dev/ttyUSB0" minitelConfigurationStandard

    m <<< seqEfface EffTout

    haskellMinitel m

    flush (serial m)
    threadDelay 100000
