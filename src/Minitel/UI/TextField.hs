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
module Minitel.UI.TextField
( TFState
, textBuffer
, cursorPosition
, insertChar
, deleteChar
, getValue
, TextField
, textField
, width
, doDraw
, doEnter
, doLeft
, doRight
, doCorrection
, doChar
, getString
, getCursorPosition
)
where

import Minitel.Type.MNatural (MNat, mnat)
import Minitel.Type.MString (MMString)
import Minitel.Generate.Generator
import Minitel.Generate.Configuration
import Minitel.Key
import Minitel.UI.Widget
       ( State (State)
       , Widget (draw, common)
       , Focusable (leave, keypress, enter)
       , CommonAttributes ( posX, posY, mode, foreground, background )
       , WithState (withState, state)
       )

import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar)

import Control.Applicative (liftA)

-- | TFState holds a text buffer for the text field widget.
--   First value is the buffer content
--   Second value is the current cursor position
type TFState = State (String, Int)

-- | Returns the string of the state
textBuffer :: TFState -> String
textBuffer (State (s, _)) = s

-- | Returns the cursor position of the state
cursorPosition :: TFState -> Int
cursorPosition (State (_, c)) = c

-- | Insert a character at a specific location in the string
insertChar :: TFState -> Char -> TFState
insertChar (State (str, pos)) c = State (insert str c pos, pos + 1)
    where insert s c' p = take p s ++ [c'] ++ drop p s

-- | Delete a character at a specific location in the string
deleteChar :: TFState -> TFState
deleteChar (State (str,pos)) = State (delete str pos, pos)
    where delete s p = take (p - 1) s ++ drop p s

-- | Pad a string into another string of a specified length.
--
--   pad 5  "abcdefghi" = "abcde"
--   pad 10 "abcde"     = "abcde     "
pad :: Int -> String -> String
pad 0 ""     = ""
pad l ""     = ' ':pad (l - 1) ""
pad 0 (_:_)  = ""
pad l (_:xs) = ' ':pad (l - 1) xs

-- | A text field widget is used to manage simple user input (an input text
--   field on one line)
data TextField = TextField CommonAttributes (MVar TFState) MNat
    deriving Eq

width :: TextField -> MNat
width (TextField _ _ w) = w

-- | Construct a new TextField given common attributes, width, initial value.
textField :: CommonAttributes -- ^ Common attributes (x, y, colors etc.)
          -> MNat             -- ^ Visible width
          -> String           -- ^ Initial value
          -> IO TextField
textField common' w init' = do
    mvState <- newMVar $ State (init', 0)
    return $ TextField common' mvState w

-- | Get value of the textfield
getValue :: TextField -> IO TFState
getValue = readMVar . state

-- | Get text of the textfield
getString :: TextField -> IO String
getString = liftA textBuffer . getValue

-- | Get cursor position of the textfield
getCursorPosition :: TextField -> IO Int
getCursorPosition = liftA cursorPosition . getValue

-- | A TextField has a state
instance WithState TextField TFState where
    state (TextField _ st _) = st
    withState widget func = modifyMVar (state widget) func'
        where func' state' = return $ func widget state'

-- | Answers to signals
instance Widget TextField where
    common (TextField cm _ _) = cm
    draw widget   = withState widget doDraw

-- | Answers to signals
instance Focusable TextField where
    enter widget  = withState widget doEnter
    leave _       = return $ Just [ mVisibleCursor False ]

    -- | Supported keys in the textfield
    keypress widget (KeyLeft Plain)       = withState widget doLeft
    keypress widget (KeyRight Plain)      = withState widget doRight
    keypress widget (KeyCorrection Plain) = withState widget doCorrection
    keypress widget (KeyAlpha c)          = withState widget (doChar c)

    -- | Every other key is ignored
    keypress _ _ = return Nothing

-- | Draw a TextField
doDraw :: TextField -> TFState -> (TFState, MMString)
doDraw tf state'@(State (s, _)) =
    ( state'
    , Just [ mLocate (posX c) (posY c)
           , mForeground (foreground c)
           , mBackground (background c)
           , mString (mode c) $ pad ((fromIntegral . width) tf) s
           ]
    )
    where c = common tf

-- | The TextField receives focus
doEnter :: TextField -> TFState -> (TFState, MMString)
doEnter tf state'@(State (_, pos)) =
    ( state', Just [ mLocate x y, mVisibleCursor True] )
    where c = common tf
          (x, y) = (fromIntegral (posX c + mnat pos), fromIntegral (posY c))

-- | Move cursor to the left
doLeft :: TextField -> TFState -> (TFState, MMString)
doLeft _ state'@(State (s, pos))
    | pos == 0  = (state', Just [ mBeep ])
    | otherwise = (State (s, pos - 1), Just [ mMove (-1) 0 ])

-- | Move cursor to the right
doRight :: TextField -> TFState -> (TFState, MMString)
doRight _ state'@(State (s, pos))
    | pos > length s = (state', Just [ mBeep ])
    | otherwise      = (State (s, pos + 1), Just [ mMove 1 0 ])

-- | Backspace key
doCorrection :: TextField -> TFState -> (TFState, MMString)
doCorrection _ state'@(State (s, pos))
    | pos == 0  = (state', Just [ mBeep ])
    | otherwise = (newState, Just [ mMove (-1) 0, [0x20], mMove (-1) 0 ])
    where newState = State (deleteNth (pos - 1) s, pos - 1)
          deleteNth _ [] = []
          deleteNth 0 (_:xs) = xs
          deleteNth n (x:xs) = x:deleteNth (n - 1) xs

-- | Insert a char
doChar :: Char -> TextField -> TFState -> (TFState, MMString)
doChar c tf (State (s, pos)) = (newState, Just [ mString m [c] ])
    where m        = mode $ common tf
          newState = State (s ++ [c], pos + 1)

