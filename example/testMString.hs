import Minitel.Minitel
import Minitel.Type.Videotex

import Minitel.Generate.Configuration
import Minitel.Generate.Generator

import System.Hardware.Serialport

main :: IO ()
main = do
    let mySettings = SerialPortSettings
            { commSpeed   = CS9600
            , bitsPerWord = 8
            , stopb       = One
            , parity      = NoParity
            , flowControl = NoFlowControl
            , timeout     = 10
            }

    m <- minitel "/dev/ttyUSB0" mySettings

    mapM_ (mCall m) [ mExtendedKeyboard  True
                    , mCursorKeys        True
                    , mLowercaseKeyboard True
                    ]

    m <<< [ mClear ReallyEverything
          , mLocate 1 0
          , mString VideoTex "Test MString"
          , mLocate 1 1
          , mString VideoTex "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          ]

    ident <- mCall m mIdentification

    print ident

    waitForMinitel m

    putStrLn "End!"

