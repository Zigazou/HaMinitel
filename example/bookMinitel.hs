import Minitel.Minitel
import Minitel.Generator
import Minitel.Queue
import Minitel.MString

import Control.Concurrent
import Control.Monad
import System.Hardware.Serialport

import Data.Char

tableOfContents :: [MString]
tableOfContents =
    [ mClear      ReallyEverything
    , mLocate     1 0
    , mString     VideoTex "Table of contents"
    , mLocate     1 2
    , mSize       DoubleSize DoubleSize
    , mForeground Green
    , mString     VideoTex "Haskell minitel"
    , mRectangle  2 4 36 15 Blue
    , mLocate     3 6
    , mForeground Yellow
    , mSize       SimpleSize DoubleSize
    , mString     VideoTex " 1- What is HaMinitel ?"
    , mLocate     3 9
    , mForeground Yellow
    , mSize       SimpleSize DoubleSize
    , mString     VideoTex " 2- How does it work ?"
    , mBeep
    ]

main = do
    m <- minitel "/dev/ttyUSB0" baseSettings
    --putStrLn "Changing speed"
    --m <- setSpeed m 300

    --putStrLn "Configuring keyboard"
    m <<< [ mExtendedKeyboard  True
          , mCursorKeys        True
          , mLowercaseKeyboard True
          ]

    --putStrLn "Sending page"
    m <<< tableOfContents

    --putStrLn "Waiting for everything to be really sent to the Minitel"
    waitForMinitel m

