{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-} 
{-|
Module      : Label
Description : Label widget for the Minitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module provides a Label widget for the Minitel. A label is a simple widget
displaying a line of text.
-}
module Minitel.UI.Label
( Justification(JLeft, JCenter, JRight)
, Label(Label)
, width
, value
, justification
, doDraw
)
where

import Minitel.Type.MNatural (MNat)
import Minitel.Type.MString (MMString)
import Minitel.Generate.Generator (mLocate, mForeground, mBackground, mString)
import Minitel.UI.Widget
       ( Widget (common, draw)
       , CommonAttributes (mode, posX, posY, foreground, background)
       )

data Justification = JLeft | JCenter | JRight
    deriving Eq

-- | A text field widget is used to manage simple user input (an input text
--   field on one line)
data Label = Label CommonAttributes -- ^ Common attributes
                   String           -- ^ Text to display
                   MNat             -- ^ Width
                   Justification    -- ^ Justification (Center, Left, Right)
                   deriving Eq

width :: Label -> MNat
width (Label _ _ w _) = w

value :: Label -> String
value (Label _ s _ _) = s

justification :: Label -> Justification
justification (Label _ _ _ j) = j

instance Widget Label where
    common (Label cm _ _ _) = cm
    draw widget = return $ doDraw widget

doDraw :: Label -> MMString
doDraw lb = Just [ mLocate (posX c) (posY c)
                 , mForeground (foreground c)
                 , mBackground (background c)
                 , mString (mode c) (value lb)
                 ]
                 where c = common lb

