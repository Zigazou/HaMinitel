{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-} 
{-|
Module      : Dispatcher
Description : Dispatcher engine for the Minitel
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module provides a Dispatcher which will dispatch an event to a list of
widget.
-}
module Minitel.UI.Dispatcher where

import Minitel.Type.MString
import Minitel.Minitel
import Minitel.Generate.Generator
import Minitel.UI.Widget
import Minitel.UI.Interface
import Minitel.Key

runInterface :: (Focusable a) => Minitel -> Interface a -> IO (Interface a)
runInterface minitel' interface@(Interface back focusables' current) = do
    -- | Draw the widgets (standards and focusables)
    minitel' <<< back
    mapM_ ((<<<) minitel' =<<) $ map draw focusables'
 
    -- | Gives focus to the current widget (the first in the focusables list)
    (<<<) minitel' =<< enter current

    -- | Run the interface
    runInterface' minitel' interface

runInterface' :: Focusable a => Minitel -> Interface a -> IO (Interface a)
runInterface' minitel' intf@(Interface back focusables' current) = do
    -- | Wait for a key from the Minitel
    ms <- readBMString (getter minitel') completeReturn

    -- | Search for a global key
    let key = toKey ms
        (nextWidget, output') = case key of
            -- | Go to next focusable widget, if available
            KeyNext Plain -> gotoWidget current (nextFocusable intf)

            -- | Go to previous focusable widget, if available
            KeyPrev Plain -> gotoWidget current (prevFocusable intf)

            -- | The interface has been completed by the user
            KeySend Plain -> (Nothing, ignored)

            -- | Send the key to the current widget
            _             -> (Just current, keypress current key)

    -- | Send the output to the Minitel
    (<<<) minitel' =<< output'

    case nextWidget of
        -- | End the loop by returning the current Interface state
        Nothing -> return (Interface back focusables' current)

        -- | Loop over the same interface (the current widget may have changed)
        Just w  -> runInterface' minitel' (Interface back focusables' w)

  where ignored :: IO MMString
        ignored = return $ Just [ mBeep ]

        gotoWidget :: (Focusable a) => a -> Maybe a -> (Maybe a, IO MMString)
        gotoWidget current' nextw = case nextw of
            Just next -> ( Just next
                         , leave current' >>= \leaving ->
                           enter next     >>= \entering ->
                           return $ leaving +++ entering
                         )
            Nothing   -> (Just current', ignored)

