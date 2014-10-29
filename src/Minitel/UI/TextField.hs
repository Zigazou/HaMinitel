{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-} 
{-|
Module      : TextField
Description : TextField widget for the Minitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module provides a TextField widget for the Minitel. A text field is an
input widget which can contain one line of user modifiable text.

A text field is in fact a small window on a string value.
-}
module Minitel.UI.TextField where

import Minitel.MString
import Minitel.Minitel
import Minitel.Generator
import Minitel.Key
import Minitel.UI.Widget
import Data.Char

import Control.Concurrent.MVar

-- | TFState holds a text buffer for the text field widget
type TFState = State (String, Int)

insertChar :: TFState -> Char -> TFState
insertChar (State (str, pos)) c = State (insert str c pos, pos + 1)
    where insert s c p = (take p s) ++ [c] ++ (drop p s)

deleteChar :: TFState -> TFState
deleteChar (State (str,pos)) = State (delete str pos, pos)
    where delete s p = (take (p - 1) s) ++ (drop p s)

-- | Pad a string into another string of a specified length.
--
--   pad 5  "abcdefghi" = "abcde"
--   pad 10 "abcde"     = "abcde     "
pad :: Int -> String -> String
pad 0 ""     = ""
pad l ""     = ' ':pad (l - 1) ""
pad 0 (x:xs) = ""
pad l (x:xs) = ' ':pad (l - 1) xs

-- | A text field widget is used to manage simple user input (an input text
--   field on one line)
data TextField = TextField CommonAttributes (MVar TFState) Int
    deriving Eq

width (TextField _ _ w) = w

-- | Construct a new TextField given common attributes, width, initial value.
newTextField :: CommonAttributes -- ^ Common attributes (x, y, colors etc.)
             -> Int              -- ^ Visible width
             -> String           -- ^ Initial value
             -> IO TextField
newTextField common w init = do
    mvState <- newMVar $ State (init, 0)
    return $ TextField common mvState w

-- | A TextField has a state
instance WithState TextField TFState where
    state (TextField _ st _) = st
    withState widget func = modifyMVar (state widget) func'
        where func' state = return $ func widget state

-- | Answers to signals
instance Widget TextField where
    common (TextField cm _ _) = cm
    draw widget   = withState widget doDraw

-- | Answers to signals
instance Focusable TextField where
    enter widget  = withState widget doEnter
    leave widget  = return $ Just [ mVisibleCursor False ]

    -- | Supported keys in the textfield
    keypress widget (KeyLeft Plain)       = withState widget doLeft
    keypress widget (KeyRight Plain)      = withState widget doRight
    keypress widget (KeyCorrection Plain) = withState widget doCorrection
    keypress widget (KeyAlpha c)          = withState widget (doChar c)

    -- | Every other key is ignored
    keypress _ _ = return Nothing

-- | Draw a TextField
doDraw :: TextField -> TFState -> (TFState, MMString)
doDraw tf state@(State (s, pos)) =
    ( state
    , Just [ mLocate (posX c) (posY c)
           , mForeground (foreground c)
           , mBackground (background c)
           , mString (mode c) $ pad (width tf) s
           ]
    )
    where c = common tf

-- | The TextField receives focus
doEnter :: TextField -> TFState -> (TFState, MMString)
doEnter tf state@(State (s, pos)) =
    ( state
    , Just [ mLocate ((posX c) + pos) (posY c)
           , mVisibleCursor True
           ]
    )
    where c = common tf

-- | Move cursor to the left
doLeft :: TextField -> TFState -> (TFState, MMString)
doLeft _ state@(State (s, pos))
    | pos == 0  = (state, Just [ mBeep ])
    | otherwise = (State (s, pos - 1), Just [ mMove (-1) 0 ])

-- | Move cursor to the right
doRight :: TextField -> TFState -> (TFState, MMString)
doRight _ state@(State (s, pos))
    | pos > (length s) = (state, Just [ mBeep ])
    | otherwise        = (State (s, pos + 1), Just [ mMove 1 0 ])

-- | Backspace key
doCorrection :: TextField -> TFState -> (TFState, MMString)
doCorrection _ state@(State (s, pos))
    | pos == 0  = (state, Just [ mBeep ])
    | otherwise = (newState, Just [ mMove (-1) 0, [0x20], mMove (-1) 0 ])
    where newState = State (deleteNth (pos - 1) s, pos - 1)
          deleteNth _ [] = []
          deleteNth 0 (x:xs) = xs
          deleteNth n (x:xs) = x:deleteNth (n - 1) xs

-- | Insert a char
doChar :: Char -> TextField -> TFState -> (TFState, MMString)
doChar c tf state@(State (s, pos)) = (newState, Just [ mString m [c] ])
    where m        = mode $ common tf
          newState = State (s ++ [c], pos + 1)

