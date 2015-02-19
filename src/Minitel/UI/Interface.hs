{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs #-} 
{-|
Module      : Interface
Description : Interface, a widget collection
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

An Interface is a collection of widgets which keeps track of the current
widget.
-}

module Minitel.UI.Interface where

import Minitel.UI.Widget
import Minitel.Type.MString

data Interface a where
    Interface :: Focusable a
              => [MString] -- ^ Background of the interface
              -> [a]       -- ^ Focusable widgets
              -> a         -- ^ Current widget
              -> Interface a

currentWidget :: Interface a -> a
currentWidget (Interface _ _ widget) = widget

focusables :: Interface a -> [a]
focusables (Interface _ fs _) = fs

newInterface :: Focusable a => [MString] -> [a] -> Interface a
newInterface back focusables' = Interface back focusables' (head focusables')

-- | Return the element preceding a specified element inside a list
elemBefore :: (Eq a) => a -> [a] -> Maybe a
elemBefore _ []   = Nothing
elemBefore _ [_]  = Nothing
elemBefore widget (p:w:ws)
    | w == widget = Just p
    | otherwise   = elemBefore widget (w:ws)

-- | Return the element following a specified element inside a list
elemAfter :: (Eq a) => a -> [a] -> Maybe a
elemAfter _ []   = Nothing
elemAfter _ [_]  = Nothing
elemAfter widget (w:n:ws)
    | w == widget = Just n
    | otherwise   = elemAfter widget (n:ws)

-- | Returns the next focusable widget from a list given a widget
nextFocusable :: Eq a => Interface a -> Maybe a
nextFocusable (Interface _ focusables' current) = elemAfter current focusables'

-- | Returns the previous focusable widget from a list given a widget
prevFocusable :: Eq a => Interface a -> Maybe a
prevFocusable (Interface _ focusables' current) = elemBefore current focusables'

