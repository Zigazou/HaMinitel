{-|
Module      : Abilities
Description : Values definitions from the Minitel universe
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

-}
module Minitel.Constants.Abilities
( minitelAbilities
, abilityUnknown
, makers
, makerUnknown
) where

import Minitel.Type.Ability (Ability (Ability), Maker (Maker))

-- | A list of known Minitels and their abilities
minitelAbilities :: [Ability]
minitelAbilities =
    [ Ability 'c' "Minitel 1"          False "ABCD"   1200 False False False
    , Ability 'd' "Minitel 10"         False "Azerty" 1200 False False False
    , Ability 'e' "Minitel 1 couleur"  False "Azerty" 1200 False False False
    , Ability 'f' "Minitel 10"         True  "Azerty" 1200 False False False
    , Ability 'g' "Émulateur"          True  "Azerty" 9600 True  True  False
    , Ability 'j' "Imprimante"         False ""       1200 False False False
    , Ability 'p' "Minitel Magis Club" True  "Azerty" 9600 True  True  True
    , Ability 'r' "Minitel 1"          True  "Azerty" 1200 False False False
    , Ability 's' "Minitel 1 couleur"  True  "Azerty" 1200 False False False
    , Ability 't' "Terminatel 252"     False ""       1200 False False False
    , Ability 'u' "Minitel 1B"         True  "Azerty" 4800 True  False False
    , Ability 'v' "Minitel 2"          True  "Azerty" 9600 True  True  False
    , Ability 'w' "Minitel 10B"        True  "Azerty" 4800 True  False False
    , Ability 'y' "Minitel 5"          True  "Azerty" 9600 True  True  False
    , Ability 'z' "Minitel 12"         True  "Azerty" 9600 True  True  False
    ]

-- | Base abilities, when you don't know the Minitel to which you are connected
abilityUnknown :: Ability
abilityUnknown =
    Ability '*' "Minitel inconnu"   False "ABCD"   1200 False False False

-- | Maker identification codes
makers :: [Maker]
makers =
    [ Maker 'A' "Matra"
    , Maker 'B' "RTIC"
    , Maker 'C' "Telic-Alcatel"
    , Maker 'D' "Thomson"
    , Maker 'E' "CCS"
    , Maker 'F' "Fiet"
    , Maker 'G' "Fime"
    , Maker 'H' "Unitel"
    , Maker 'I' "Option"
    , Maker 'J' "Bull"
    , Maker 'K' "Télématique"
    , Maker 'L' "Desmet"
    ]

-- | Unknown maker
makerUnknown :: Maker
makerUnknown = Maker '*' "Fabricant inconnu"

