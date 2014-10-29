import Minitel.Minitel
import Minitel.Generator
import Minitel.Queue
import Minitel.MString

import Minitel.UI.Widget
import Minitel.UI.Interface
import Minitel.UI.Dispatcher
import Minitel.UI.Label
import Minitel.UI.TextField

import Control.Concurrent
import Control.Monad
import System.Hardware.Serialport

import Data.Char

-- | Base attributes for widgets
att = CommonAttributes
    { mode       = VideoTex
    , posX       = 1
    , posY       = 1
    , foreground = White
    , background = Blue
    }

back :: [MString]
back =
    [ mVisibleCursor True
    , mClear         ReallyEverything

    , mLocate        1 0
    , mString        VideoTex "HaMinitel, Haskell’s Minitel library"

    , mLocate        1 2
    , mSize          DoubleSize DoubleSize
    , mForeground    Green
    , mString        VideoTex "Interface demo"

    , mRectangle     2 4 36 15 Blue

    , mLocate        3 5
    , mSize          DoubleSize SimpleSize
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

focuss =
    [ newTextField att { posX = 17, posY = 8  } 15 ""
    , newTextField att { posX = 17, posY = 10 } 15 ""
    , newTextField att { posX = 17, posY = 12 } 3  ""
    ]

main = do
    m <- minitel "/dev/ttyUSB0" baseSettings

    m <<< [ mExtendedKeyboard  True
          , mCursorKeys        True
          , mLowercaseKeyboard True
          , mEchoing           False
          ]

    fss <- sequence focuss

    let interface = newInterface back fss
    interface <- runInterface m interface
    
    waitForMinitel m

