import Minitel.Minitel
import Minitel.Type.Videotex

import Minitel.Generate.Configuration
import Minitel.Generate.Generator
import Minitel.Type.Ability
import Minitel.Type.MString
import Minitel.Constants.Abilities
import qualified Minitel.Constants.C0 as C0

import System.Hardware.Serialport

mInfo :: (Maybe Maker, Maybe Ability, Char) -> [MString]
mInfo (Nothing, a, c) = mInfo (Just makerUnknown, a, c)
mInfo (m, Nothing, c) = mInfo (m, Just abilityUnknown, c)
mInfo (Just m, Just a, _) =
    [ mString VideoTex "Maker"
    , [C0.APR, C0.APD]
    , mRepeat 40 0x60
    , [C0.APD]
    , mSize DoubleWidth DoubleHeight
    , mString VideoTex (makerName m)
    , [C0.APR, C0.APD, C0.APD, C0.APD]
    , mSize SimpleWidth SimpleHeight
    , mString VideoTex "Model"
    , [C0.APR, C0.APD]
    , mRepeat 40 0x60
    , [C0.APD]
    , mSize DoubleWidth DoubleHeight
    , mString VideoTex (abilityName a)
    ]

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

    m <- minitel "/dev/ttyUSB0" baseSettings
    {-
    m <- minitel "/dev/ttyS0" baseSettings

    m <<< mString VideoTex "ATA\r"

    _ <- waitForConnection m
    -}
    {-
    mapM_ (mCall m) [ mExtendedKeyboard  True
                    , mCursorKeys        True
                    , mLowercaseKeyboard True
                    ]
    -}

    m <<< [ mClear ReallyEverything
          , mLocate 1 0
          , mString VideoTex "Minitel information"
          , mLocate 1 1
          , mString VideoTex "Et c’est parti !"
          ]

    m <<< mLocate 1 3

    info <- mCall m mIdentification
    
    ((m <<<) . mInfo . minitelInfo) info

    waitForMinitel m

    waitForMinitel m

    putStrLn "End!"

