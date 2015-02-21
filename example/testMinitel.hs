import Minitel.Minitel
import Minitel.Generator
import Minitel.Queue
import Minitel.MString

import Control.Concurrent
import Control.Monad
import System.Hardware.Serialport

import Data.Char

haskellMinitel :: Minitel -> IO ()
haskellMinitel m = forM_ [2,4 .. 20] $ \ x ->
    m <<< [ mLocate (x - 1) x
          , mSize SimpleSize DoubleSize
          , mString VideoTex "Haskell Minitel!"
          ]

main = do
    m <- minitel "/dev/ttyUSB0" baseSettings

    mapM_ (mCall m) [ mExtendedKeyboard  True
                    , mCursorKeys        True
                    , mLowercaseKeyboard True
                    ]

    m <<< [ mClear ReallyEverything
          , mLocate 1 0
          , mString VideoTex "Démonstration de HaMinitel"
          ]

    haskellMinitel m

    forever $ do
        s <- readMString (get $ input m) completeReturn
        print s

    flush (serial m)
