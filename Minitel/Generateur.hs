-- Module Generateur
module Minitel.Generateur where

import Minitel.Constantes
import Data.Char
import Minitel.Sequence

data ModeMinitel = VideoTex | Mixte | TeleInformatique
    deriving (Ord, Eq, Show)

type CouleurMinitel = Integer

class ToCouleurMinitel a where
    toCouleurMinitel :: a -> CouleurMinitel

data Couleur = Noir | Rouge | Vert | Jaune | Bleu | Magenta | Cyan | Blanc
    deriving (Ord, Eq, Show)

data Gris = Gris0 | Gris1 | Gris2 | Gris3 | Gris4 | Gris5 | Gris6 | Gris7
    deriving (Ord, Eq, Show)

instance ToCouleurMinitel Couleur where
    toCouleurMinitel a = case a of
        Noir    -> 0
        Rouge   -> 1
        Vert    -> 2
        Jaune   -> 3
        Bleu    -> 4
        Magenta -> 5
        Cyan    -> 6
        Blanc   -> 7

instance ToCouleurMinitel Gris where
    toCouleurMinitel a = case a of
        Gris0 -> 0
        Gris1 -> 4
        Gris2 -> 1
        Gris3 -> 5
        Gris4 -> 2
        Gris5 -> 6
        Gris6 -> 3
        Gris7 -> 7

instance Read Couleur where
    readsPrec _ s
        | s == "noir"    = [(Noir, "noir")]
        | s == "rouge"   = [(Rouge, "rouge")]
        | s == "vert"    = [(Vert, "vert")]
        | s == "jaune"   = [(Jaune, "jaune")]
        | s == "bleu"    = [(Bleu, "bleu")]
        | s == "magenta" = [(Magenta, "magenta")]
        | s == "cyan"    = [(Cyan, "cyan")]
        | s == "blanc"   = [(Blanc, "blanc")]
        | otherwise = []

seqMode :: ModeMinitel -> ModeMinitel -> SeqValide
seqMode TeleInformatique VideoTex = (csi ++ [0x3f, 0x7b], [sep, 0x5e])
seqMode VideoTex Mixte            = (pro2 ++ mixte1, [sep, 0x70])
seqMode VideoTex TeleInformatique = (pro2 ++ telinfo, csi ++ [0x3f, 0x7a])
seqMode Mixte VideoTex            = (pro2 ++ mixte2, [sep, 0x71])
seqMode Mixte TeleInformatique    = seqMode VideoTex TeleInformatique

seqString :: ModeMinitel -> String -> SeqMinitel
seqString VideoTex s = concatMap versVideotex s
seqString _ s = concatMap versAutre s

seqIdentification :: SeqAppel
seqIdentification = (pro1 ++ [enqrom], 5)

seqClavierEtendu :: Bool -> SeqAppel
seqClavierEtendu True  = (pro3 ++ [start, rcptClavier, eten], longueurPro3)
seqClavierEtendu False = (pro3 ++ [stop , rcptClavier, eten], longueurPro3)

seqClavierCurseur :: Bool -> SeqAppel
seqClavierCurseur True  = (pro3 ++ [start, rcptClavier, c0], longueurPro3)
seqClavierCurseur False = (pro3 ++ [stop , rcptClavier, c0], longueurPro3)

seqClavierMinuscule :: Bool -> SeqAppel
seqClavierMinuscule True  = (pro2 ++ [start, minuscules], longueurPro2)
seqClavierMinuscule False = (pro2 ++ [stop , minuscules], longueurPro2)

seqAvantPlan :: (ToCouleurMinitel a) => a -> SeqMinitel
seqAvantPlan couleur = [esc, 0x40 + toCouleurMinitel couleur]

seqArrierePlan :: (ToCouleurMinitel a) => a -> SeqMinitel
seqArrierePlan couleur = [esc, 0x50 + toCouleurMinitel couleur]

seqPosition :: Integer -> Integer -> SeqMinitel
seqPosition 1 1 = [rs]
seqPosition x y = [us, 0x40 + y, 0x40 + x]

seqPositionR :: Integer -> Integer -> SeqMinitel
seqPositionR 0 0 = []
seqPositionR 0 y
    | y >= -4 && y <= -1 = replicate (fromIntegral . abs $ y) vt
    | y >= 1  && y <=  4 = replicate (fromIntegral y) lf
    | y <  0             = csi ++ seqIntString y ++ [0x42]
    | y >  0             = csi ++ seqIntString y ++ [0x41]
seqPositionR x 0
    | x >= -4 && x <= -1 = replicate (fromIntegral . abs $ x) bs
    | x >= 1  && x <=  4 = replicate (fromIntegral x) tab
    | x <  0             = csi ++ seqIntString x ++ [0x43]
    | x >  0             = csi ++ seqIntString x ++ [0x44]
seqPositionR x y = seqPositionR x 0 ++ seqPositionR 0 y

seqTaille :: Integer -> Integer -> SeqMinitel
seqTaille largeur hauteur = [esc, 0x4c + (hauteur - 1) + (largeur - 1) * 2]

seqSoulignement :: Bool -> SeqMinitel
seqSoulignement True  = [esc, 0x5a]
seqSoulignement False = [esc, 0x59]

seqClignotement :: Bool -> SeqMinitel
seqClignotement True  = [esc, 0x48]
seqClignotement False = [esc, 0x49]

seqInversion :: Bool -> SeqMinitel
seqInversion True  = [esc, 0x5d]
seqInversion False = [esc, 0x5c]

seqCurseurVisible :: Bool -> SeqMinitel
seqCurseurVisible True  = [con]
seqCurseurVisible False = [cof]

seqEchoActif :: Bool -> SeqAppel
seqEchoActif True  =
    (pro3 ++ [ aiguillageOn , rcptEcran, emetModem], longueurPro3)
seqEchoActif False =
    (pro3 ++ [ aiguillageOff, rcptEcran, emetModem], longueurPro3)

data Effacement = EffTout
                | EffFinLigne
                | EffFinEcran
                | EffDebutEcran
                | EffDebutLigne
                | EffLigne
                | EffStatut
                | EffVraimentTout

seqEfface :: Effacement -> SeqMinitel
seqEfface EffTout         = [ff]
seqEfface EffFinLigne     = [can]
seqEfface EffFinEcran     = csi ++ [0x4a]
seqEfface EffDebutEcran   = csi ++ [0x31, 0x4a]
seqEfface EffDebutLigne   = csi ++ [0x31, 0x4b]
seqEfface EffLigne        = csi ++ [0x32, 0x4b]
seqEfface EffStatut       = [us, 0x40, 0x41, can, lf]
seqEfface EffVraimentTout = seqEfface EffTout ++ seqEfface EffStatut

seqRepeter :: Integer -> Integer -> SeqMinitel
seqRepeter nombre c = [c, rep, 0x40 + nombre - 1]

seqBip :: SeqMinitel
seqBip = [bel]

seqDebutLigne :: SeqMinitel
seqDebutLigne = [cr]

seqSupprimeColonne :: Integer -> SeqMinitel
seqSupprimeColonne nb = csi ++ seqIntString nb ++ [0x50]

seqSupprimeLigne :: Integer -> SeqMinitel
seqSupprimeLigne nb = csi ++ seqIntString nb ++ [0x4d]

seqInsereColonne :: Integer -> SeqMinitel
seqInsereColonne nb = csi ++ [0x34, 0x68] ++ replicate (fromIntegral nb) 0x20
                   ++ csi ++ [0x34, 0x6c]

seqInsereLigne :: Integer -> SeqMinitel
seqInsereLigne nb = csi ++ seqIntString nb ++ [0x4c]

seqSemigraphiqueActif :: Bool -> SeqMinitel
seqSemigraphiqueActif True  = [so]
seqSemigraphiqueActif False = [si]

-- Redéfinition de caractères
type DessinCaractere = [String]
data Jeu = G0 | G1

seqDefinitionJeu :: Jeu -> SeqMinitel
seqDefinitionJeu G0 = [us, 0x23, 0x20, 0x20, 0x20, 0x42, 0x49]
seqDefinitionJeu G1 = [us, 0x23, 0x20, 0x20, 0x20, 0x43, 0x49]

seqSelectionJeu :: Jeu -> SeqMinitel
seqSelectionJeu G0 = [esc, 0x28, 0x20, 0x42]
seqSelectionJeu G1 = [esc, 0x29, 0x20, 0x43]

seqDessin :: [String] -> SeqMinitel
seqDessin dessin = map bits' ((multiSplit 6 . concat) dessin) ++ [0x30]
    where bits = foldl (\a v -> 2 * a + (if v == '1' then 1 else 0)) 0
          bits' s = 0x40 + bits s * (if length s == 2 then 16 else 1)
          multiSplit _ [] = []
          multiSplit nb s = start:multiSplit nb end
              where (start, end) = splitAt nb s

seqDessins :: [[String]] -> [SeqMinitel]
seqDessins = map seqDessin

seqRedefinir :: Integer -> [DessinCaractere] -> Jeu -> SeqMinitel
seqRedefinir depuis dessins jeu =
    seqDefinitionJeu jeu
    ++ (concat . seqDessins) dessins
    ++ [us, 0x41, 0x41]
    ++ seqSelectionJeu jeu

