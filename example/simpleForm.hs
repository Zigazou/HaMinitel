{-|
Module      : SimpleForm
Description : Demo of a simple form using HaMinitel
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This example is a demo of a simple form using HaMinitel.
It consists of 3 input fields in front of a background. 
-}
module Main where

-- Detailed imports to help you know the roles of each part. 
import Minitel.Minitel (minitel, (<<<), waitForMinitel, baseSettings)
import Minitel.Generate.Generator
       ( mClear, mLocate, mString, mSize, mForeground, mRectangle, mRepeat )
import Minitel.Generate.Configuration
       ( mVisibleCursor, mExtendedKeyboard, mCursorKeys, mLowercaseKeyboard
       , mEchoing
       )
import Minitel.Type.MString (MString)
import Minitel.Type.Videotex
       ( WhatToClear (ReallyEverything)
       , MMode (VideoTex)
       , CharWidth (DoubleWidth)
       , CharHeight (DoubleHeight, SimpleHeight)
       , Color (Green, Blue, Yellow, White)
       )
import Minitel.UI.Widget
       ( CommonAttributes
         ( CommonAttributes, posX, posY, mode, foreground, background )
       )
import Minitel.UI.Interface (interface, focusables)
import Minitel.UI.Dispatcher (runInterface)
import Minitel.UI.TextField (TextField, textField, getString)

-- | Defines the background of our textual user interface
formBackground :: [MString]
formBackground =
    [ mVisibleCursor True
    , mClear         ReallyEverything

    -- Writes on the status line
    , mLocate        1 0
    , mString        VideoTex "HaMinitel, Haskell’s Minitel library"

    -- Title
    , mLocate        1 2
    , mSize          DoubleWidth DoubleHeight
    , mForeground    Green
    , mString        VideoTex "Interface demo"

    -- Form background
    , mRectangle     2 4 36 15 Blue

    , mLocate        3 5
    , mSize          DoubleWidth SimpleHeight
    , mString        VideoTex "Tell me about you"

    -- Draws a line using a character
    , mLocate        3 6
    , mRepeat        34 0x5f

    -- Label for the first name field
    , mLocate        3 8
    , mForeground    Yellow
    , mString        VideoTex "First name:"

    -- Label for the last name field
    , mLocate        3 10
    , mForeground    Yellow
    , mString        VideoTex "Last name:"

    -- Label for the age field
    , mLocate        3 12
    , mForeground    Yellow
    , mString        VideoTex "Age:"
    ]

-- | Defines the interactive elements of our textual user interface.
--   There are 3 text fields
formInputs :: [IO TextField]
formInputs =
    [ textField attributes { posX = 17, posY = 8  } 15 ""
    , textField attributes { posX = 17, posY = 10 } 15 ""
    , textField attributes { posX = 17, posY = 12 } 3  ""
    ]
    -- All fields will use VideoTex mode and are written white on blue
    where attributes = CommonAttributes { mode       = VideoTex
                                        , posX       = 1
                                        , posY       = 1
                                        , foreground = White
                                        , background = Blue
                                        }

main :: IO ()
main = do
    -- Connect to the Minitel with standard settings. /dev/ttyUSB0 is the
    -- standard port when using USB UART based on FT232 chipset.
    m <- minitel "/dev/ttyUSB0" baseSettings

    -- Configure the Minitel keyboard. By default, the Minitel keyboard outputs
    -- uppercase letters, echoes every key and does not listen to special keys.
    m <<< [ mExtendedKeyboard  True
          , mCursorKeys        True
          , mLowercaseKeyboard True
          , mEchoing           False
          ]

    -- Create and run our simple form
    -- Text fields use MVar to store their content, therefore they are
    -- IO tainted.
    formInputs' <- sequence formInputs
    results <- runInterface m (interface formBackground formInputs')

    -- Display results
    mapM_ ((print =<<) . getString) (focusables results)

    -- Wait till the Minitel has received everything.
    -- It is needed because the serial data transmitted may be cached at various
    -- levels (HaMinitel, OS, modem...)
    waitForMinitel m

