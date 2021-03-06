{-# LANGUAGE MultiParamTypeClasses #-} 
{-|
Module      : Widget
Description : Widget for the Minitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module provides a base widget model for the Minitel.
-}
module Minitel.UI.Widget
( State(State)
, CommonAttributes(CommonAttributes, mode, posX, posY, foreground, background)
, Widget(common, draw)
, Focusable(enter, leave, keypress)
, WithState(state, withState)
)
where

import Minitel.Type.MString (MMString)
import Minitel.Type.MNatural (MNat)
import Minitel.Type.Videotex (MMode, Color)
import Minitel.Key (Key)

import Control.Concurrent.MVar (MVar)

-- | Widgets may have some state attached to them
data State a = State a deriving (Eq, Show)

-- | Widgets all have common attributes
data CommonAttributes = CommonAttributes
    { mode       :: MMode   -- ^ Minitel Mode (VideoTex, TeleInformatique...)
    , posX       :: MNat    -- ^ X position
    , posY       :: MNat    -- ^ Y position
    , foreground :: Color   -- ^ Foreground color
    , background :: Color   -- ^ Background color
    }
    deriving Eq

-- | A widget must comply to these signals. A widget may return Nothing to a
--   signal.
class (Eq a) => Widget a where
    common    :: a -> CommonAttributes   -- ^ Common attributes 
    draw      :: a -> IO MMString        -- ^ Widget is to be drawn

class (Widget a) => Focusable a where
    enter     :: a -> IO MMString        -- ^ Widget is the current one
    leave     :: a -> IO MMString        -- ^ Widget is no more the current one
    keypress  :: a -> Key -> IO MMString -- ^ A key has been pressed

-- | Widgets having a state must comply to these functions.
class (Widget a) => WithState a b where
    -- | Returns the state of the widget
    state     :: a -> MVar b

    -- | The withState function is a helper function to handle signals which
    --   might modify the Widget state. The function passed to withState takes
    --   the widget and its current state. It should return a tuple with the
    --   new state (or the same state) and an MMString which is to be sent to
    --   the Minitel. The withState function takes care of updating the state,
    --   it thus helps the called function to remain pure.
    withState :: a
              -> (a -> b -> (b, MMString))
              -> IO MMString

