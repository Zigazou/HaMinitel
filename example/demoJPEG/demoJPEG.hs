import Minitel.Minitel
import Minitel.Type.Videotex

import Minitel.Generate.Configuration
import Minitel.Generate.Generator
import Minitel.Generate.PhotoVideotex

import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
    m <- minitel "/dev/ttyUSB0" photoSettings

    mapM_ (mCall m) [ mExtendedKeyboard  True
                    , mCursorKeys        True
                    , mLowercaseKeyboard True
                    ]

    m <<< [ mClear ReallyEverything
          , mLocate 1 0
          , mString VideoTex "PhotoVideotex demonstration"
          , mLocate 1 10
          , mSize DoubleWidth DoubleHeight
          , mString VideoTex "Minitel"
          , mLocate 3 12
          , mSize SimpleWidth DoubleHeight
          , mString VideoTex "Magis Club"
          ]

    jpg <- BS.readFile "/home/fred/dev/haskell/HaMinitel/example/image3.jpg"

    m <<< mJPEG jpg 0.425 0.0

    waitForMinitel m

    putStrLn "End!"

