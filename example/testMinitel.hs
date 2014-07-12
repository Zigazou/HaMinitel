import Minitel.Minitel
import Minitel.Generator
import Minitel.Queue
import Minitel.MString

import Control.Concurrent
import Control.Monad
import System.Hardware.Serialport

haskellMinitel :: Minitel -> IO ()
haskellMinitel m = forM_ [1 .. 20] $ \ x -> do
    m <<< mLocate x x
    m <<< mString VideoTex "Haskell Minitel!"

main = do
    m <- minitel "/dev/ttyUSB0" baseSettings

    mapM_ (mCall m) [
        mExtendedKeyboard  True,
        mCursorKeys        True,
        mLowercaseKeyboard True
        ]

    mapM_ (m <<<) [
        mClear ReallyEverything,
        mLocate 1 0,
        mString VideoTex "Démonstration de HaMinitel"
        ]

    haskellMinitel m

    forever $ do
        s <- readMString (get $ input m) completeReturn
        putStrLn $ show s

    flush (serial m)
