import Minitel.Minitel
import Minitel.Generate.Generator
import Minitel.Generate.Configuration
import Minitel.Type.MString
import Minitel.Type.Videotex

import Minitel.UI.Widget
import Minitel.UI.Interface
import Minitel.UI.Dispatcher
import Minitel.UI.TextField

-- | Defines the background of our textual user interface
back :: [MString]
back =
    [ mVisibleCursor True
    , mClear         ReallyEverything

    , mLocate        1 0
    , mString        VideoTex "HaMinitel, Haskell’s Minitel library"

    , mLocate        1 2
    , mSize          DoubleWidth DoubleHeight
    , mForeground    Green
    , mString        VideoTex "Interface demo"

    , mRectangle     2 4 36 15 Blue

    , mLocate        3 5
    , mSize          DoubleWidth SimpleHeight
    , mString        VideoTex "Tell me about you"

    , mLocate        3 6
    , mRepeat        34 0x5f

    , mLocate        3 8
    , mForeground    Yellow
    , mString        VideoTex "First name:"

    , mLocate        3 10
    , mForeground    Yellow
    , mString        VideoTex "Last name:"

    , mLocate        3 12
    , mForeground    Yellow
    , mString        VideoTex "Age:"
    ]

-- | Defines the interactive elements of our textual user interface
focuss :: [IO TextField]
focuss =
    [ newTextField att { posX = 17, posY = 8  } 15 ""
    , newTextField att { posX = 17, posY = 10 } 15 ""
    , newTextField att { posX = 17, posY = 12 } 3  ""
    ]
    where att = CommonAttributes { mode       = VideoTex
                                 , posX       = 1
                                 , posY       = 1
                                 , foreground = White
                                 , background = Blue
                                 }

main :: IO ()
main = do
    m <- minitel "/dev/ttyUSB0" baseSettings

    m <<< [ mExtendedKeyboard  True
          , mCursorKeys        True
          , mLowercaseKeyboard True
          , mEchoing           False
          ]

    fss <- sequence focuss
    _ <- runInterface m (newInterface back fss)
    
    waitForMinitel m

