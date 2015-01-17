import Minitel.Minitel
import Minitel.Type.Videotex

import Minitel.Generate.Configuration
import Minitel.Generate.Generator
import Minitel.Type.Ability
import Minitel.Type.MString
import Minitel.Constants.Abilities
import Minitel.Constants.Constants

import System.Hardware.Serialport

mInfo :: (Maybe Maker, Maybe Ability, Char) -> [MString]
mInfo (Nothing, a, c) = mInfo (Just makerUnknown, a, c)
mInfo (m, Nothing, c) = mInfo (m, Just abilityUnknown, c)
mInfo (Just m, Just a, _) =
    [ mString VideoTex "Maker"
    , [eAPR, eAPD]
    , mRepeat 40 0x60
    , [eAPD]
    , mSize DoubleWidth DoubleHeight
    , mString VideoTex (makerName m)
    , [eAPR, eAPD, eAPD, eAPD]
    , mSize SimpleWidth SimpleHeight
    , mString VideoTex "Model"
    , [eAPR, eAPD]
    , mRepeat 40 0x60
    , [eAPD]
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

    mapM_ (mCall m) [ mExtendedKeyboard  True
                    , mCursorKeys        True
                    , mLowercaseKeyboard True
                    ]

    m <<< [ mClear ReallyEverything
          , mLocate 1 0
          , mString VideoTex "Minitel information"
          , mLocate 1 1
          ]

    m <<< mLocate 1 3

    info <- mCall m mIdentification
    
    print info
    
    ((m <<<) . mInfo . minitelInfo) info

    waitForMinitel m

    m <<< [aUS, 0x20, 0x40]

    tfi <- readNCount (getter m) 1024
    
    print tfi

    putStrLn "End!"

