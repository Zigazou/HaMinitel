{-|
Module      : Abilities
Description : Values definitions from the Minitel universe
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

-}
module Minitel.Constants.Abilities where

import           Minitel.Type.Ability

-- | A list of known Minitels and their abilities
minitelAbilities :: [Ability]
minitelAbilities =
    [ Ability 'c' "Minitel 1"         False "ABCD"   1200 False False
    , Ability 'd' "Minitel 10"        False "Azerty" 1200 False False
    , Ability 'e' "Minitel 1 couleur" False "Azerty" 1200 False False
    , Ability 'f' "Minitel 10"        True  "Azerty" 1200 False False
    , Ability 'g' "Émulateur"         True  "Azerty" 9600 True  True
    , Ability 'j' "Imprimante"        False ""       1200 False False
    , Ability 'r' "Minitel 1"         True  "Azerty" 1200 False False
    , Ability 's' "Minitel 1 couleur" True  "Azerty" 1200 False False
    , Ability 't' "Terminatel 252"    False ""       1200 False False
    , Ability 'u' "Minitel 1B"        True  "Azerty" 4800 True  False
    , Ability 'v' "Minitel 2"         True  "Azerty" 9600 True  True
    , Ability 'w' "Minitel 10B"       True  "Azerty" 4800 True  False
    , Ability 'y' "Minitel 5"         True  "Azerty" 9600 True  True
    , Ability 'z' "Minitel 12"        True  "Azerty" 9600 True  True
    ]

-- | Base abilities, when you don't know the Minitel to which you are connected
baseAbilities :: Ability
baseAbilities =
    Ability '*' "Minitel inconnu"   False "ABCD"   1200 False False

-- | Maker identification codes
makerCodes :: [(Char, String)]
makerCodes =
    [ ('A', "Matra")
    , ('B', "RTIC")
    , ('C', "Telic-Alcatel")
    , ('D', "Thomson")
    , ('E', "CCS")
    , ('F', "Fiet")
    , ('G', "Fime")
    , ('H', "Unitel")
    , ('I', "Option")
    , ('J', "Bull")
    , ('K', "Télématique")
    , ('L', "Desmet")
    ]

