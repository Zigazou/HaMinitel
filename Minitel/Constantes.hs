module Minitel.Constantes where

-- Definitions de constantes de l'univers Minitel
import Data.Char

-- Codes de controles de la norme ASCII
nul = 0x00 -- null
soh = 0x01 -- start of heading
stx = 0x02 -- start of text
etx = 0x03 -- end of text
eot = 0x04 -- end of transmission
enq = 0x05 -- enquiry
ack = 0x06 -- acknowledge
bel = 0x07 -- bell
bs  = 0x08 -- backspace
tab = 0x09 -- horizontal tab
lf  = 0x0a -- line feed, new line
vt  = 0x0b -- vertical tab
ff  = 0x0c -- form feed, new page
cr  = 0x0d -- carriage return
so  = 0x0e -- shift out
si  = 0x0f -- shift in
dle = 0x10 -- data link escape
dc1 = 0x11 -- device control 1
con = 0x11 -- Cursor on
dc2 = 0x12 -- device control 2
rep = 0x12 -- Rep
dc3 = 0x13 -- device control 3
sep = 0x13 -- Sep
dc4 = 0x14 -- device control 4
cof = 0x14 -- Cursor off
nak = 0x15 -- negative acknowledge
syn = 0x16 -- synchronous idle
etb = 0x17 -- end of transmission block
can = 0x18 -- cancel
em  = 0x19 -- end of medium
ss2 = 0x19 -- SS2
sub = 0x1a -- substitute
esc = 0x1b -- escape
fs  = 0x1c -- file separator
gs  = 0x1d -- group separator
ss3 = 0x1d -- SS3
rs  = 0x1e -- record separator
us  = 0x1f -- unit separator

pro1 = [esc, 0x39] -- protocole 1
pro2 = [esc, 0x3a] -- protocole 2
pro3 = [esc, 0x3b] -- protocole 3
csi  = [esc, 0x5b] -- CSI

-- Commandes PRO1
deconnexion = 0x67
connexion = 0x68
ret1 = 0x6c
ret2 = 0x6d
oppo = 0x6f
statusTerminal = 0x70
statusClavier = 0x72
statusFonctionnement = 0x72
statusVitesse = 0x74
statusProtocole = 0x76
enqrom = 0x7b
reset = 0x7f

-- Commandes PRO2
copie = 0x7c
aiguillageTo = 0x62
nonDiffusion = 0x64
nonRetourAcquittement = 0x64
diffusion = 0x65
retourAcquittement = 0x65
transparence = 0x66
start = 0x69
stop = 0x6a
prog = 0x6b
repStatusClavier = 0x73
repStatusFonctionnement = 0x73
repStatusProtocole = 0x77
telinfo = [0x31, 0x7d]
mixte1 = [0x32, 0x7d]
mixte2 = [0x32, 0x7e]

-- Commandes PRO3
aiguillageOff = 0x60
aiguillageOn = 0x61
aiguillageFrom = 0x63

-- Longueurs commandes PRO
longueurPro1 = 3
longueurPro2 = 4
longueurPro3 = 5

-- Autres codes
copieFrancais = 0x6a
copieAmericain = 0x6b
eten = 0x41
c0 = 0x43

-- Codes PRO2+START/STOP
rouleau = 0x43
procedure = 0x44
minuscules = 0x45

-- Codes PRO2+PROG
b9600 = 0x7f
b4800 = 0x76
b1200 = 0x64
b300 = 0x52

-- Codes PRO3+START/STOP
-- eten = 0x41
-- c0 = 0x43

-- Codes de reception
rcptEcran = 0x58
rcptClavier = 0x59
rcptModem = 0x5a
rcptPrise = 0x5b
rcptTelephonique = 0x5c
rcptLogiciel = 0x5d

-- Codes d'emission
emetEcran = 0x50
emetClavier = 0x51
emetModem = 0x52
emetPrise = 0x53
emetTelephonique = 0x54
emetLogiciel = 0x55

-- Accents
accentCedille = [ss2, 0x4b]
accentGrave = [ss2, 0x41]
accentAigu = [ss2, 0x42]
accentCirconflexe = [ss2, 0x43]
accentTrema = [ss2, 0x48]

-- Touches de direction
haut = csi ++ [0x41]
bas = csi ++ [0x42]
gauche = csi ++ [0x44]
droite = csi ++ [0x43]

majHaut = csi ++ [0x4d]
majBas = csi ++ [0x4c]
majGauche = csi ++ [0x50]
majDroite = csi ++ [0x34, 0x68]

ctrlGauche = 0x7f

-- Touche Entree/Retour chariot
entree      = 0x0d
majEntree  = csi ++ [0x48]
ctrlEntree = csi ++ [0x32, 0x4a]

-- Touches de fonction
kEnvoi      = [dc3, 0x41]
kRetour     = [dc3, 0x42]
kRepetition = [dc3, 0x43]
kGuide      = [dc3, 0x44]
kAnnulation = [dc3, 0x45]
kSommaire   = [dc3, 0x46]
kCorrection = [dc3, 0x47]
kSuite      = [dc3, 0x48]
kConnexion  = [dc3, 0x49]

-- Types de minitels
data Capacite = Capacite {
    id :: Char,
    nom :: String,
    retournable :: Bool,
    clavier :: String,
    vitesse :: Int,
    colonnes80 :: Bool,
    caracteres :: Bool
} deriving (Show)

typeMinitels = [
    Capacite 'c' "Minitel 1"         False "ABCD"   1200 False False,
    Capacite 'd' "Minitel 10"        False "Azerty" 1200 False False,
    Capacite 'e' "Minitel 1 couleur" False "Azerty" 1200 False False,
    Capacite 'f' "Minitel 10"        True  "Azerty" 1200 False False,
    Capacite 'g' "Émulateur"         True  "Azerty" 9600 True  True,
    Capacite 'j' "Imprimante"        False ""       1200 False False,
    Capacite 'r' "Minitel 1"         True  "Azerty" 1200 False False,
    Capacite 's' "Minitel 1 couleur" True  "Azerty" 1200 False False,
    Capacite 't' "Terminatel 252"    False ""       1200 False False,
    Capacite 'u' "Minitel 1B"        True  "Azerty" 4800 True  False,
    Capacite 'v' "Minitel 2"         True  "Azerty" 9600 True  True,
    Capacite 'w' "Minitel 10B"       True  "Azerty" 4800 True  False,
    Capacite 'y' "Minitel 5"         True  "Azerty" 9600 True  True,
    Capacite 'z' "Minitel 12"        True  "Azerty" 9600 True  True
    ]

-- Capacites les plus basiques du Minitel
minitelBasique =
    Capacite '*' "Minitel inconnu"   False "ABCD"   1200 False False

-- Codes d'identification du constructeur
constructeurs = [
    ('A', "Matra"),
    ('B', "RTIC"),
    ('C', "Telic-Alcatel"),
    ('D', "Thomson"),
    ('E', "CCS"),
    ('F', "Fiet"),
    ('G', "Fime"),
    ('H', "Unitel"),
    ('I', "Option"),
    ('J', "Bull"),
    ('K', "Télématique"),
    ('L', "Desmet")
    ]

-- Tables de conversion des caracteres speciaux
versVideotex :: Char -> [Integer]
versVideotex c 
    | c == '£'  = [0x19, 0x23]
    | c == '°'  = [0x19, 0x30]
    | c == '±'  = [0x19, 0x31] 
    | c == '←'  = [0x19, 0x2C]
    | c == '↑'  = [0x19, 0x2D]
    | c == '→'  = [0x19, 0x2E]
    | c == '↓'  = [0x19, 0x2F] 
    | c == '¼'  = [0x19, 0x3C]
    | c == '½'  = [0x19, 0x3D]
    | c == '¾'  = [0x19, 0x3E] 
    | c == 'ç'  = [0x19, 0x4B, 0x63]
    | c == '’'  = [0x19, 0x4B, 0x27] 
    | c == 'à'  = [0x19, 0x41, 0x61]
    | c == 'á'  = [0x19, 0x42, 0x61]
    | c == 'â'  = [0x19, 0x43, 0x61]
    | c == 'ä'  = [0x19, 0x48, 0x61] 
    | c == 'è'  = [0x19, 0x41, 0x65]
    | c == 'é'  = [0x19, 0x42, 0x65]
    | c == 'ê'  = [0x19, 0x43, 0x65]
    | c == 'ë'  = [0x19, 0x48, 0x65] 
    | c == 'ì'  = [0x19, 0x41, 0x69]
    | c == 'í'  = [0x19, 0x42, 0x69]
    | c == 'î'  = [0x19, 0x43, 0x69]
    | c == 'ï'  = [0x19, 0x48, 0x69] 
    | c == 'ò'  = [0x19, 0x41, 0x6F]
    | c == 'ó'  = [0x19, 0x42, 0x6F]
    | c == 'ô'  = [0x19, 0x43, 0x6F]
    | c == 'ö'  = [0x19, 0x48, 0x6F] 
    | c == 'ù'  = [0x19, 0x41, 0x75]
    | c == 'ú'  = [0x19, 0x42, 0x75]
    | c == 'û'  = [0x19, 0x43, 0x75]
    | c == 'ü'  = [0x19, 0x48, 0x75] 
    | c == 'Œ'  = [0x19, 0x6A]
    | c == 'œ'  = [0x19, 0x7A] 
    | c == 'ß'  = [0x19, 0x7B]
    | c == 'β'  = [0x19, 0x7B]
    | isAscii c = [(fromIntegral . ord) c]
    | otherwise = []

versAutre :: Char -> [Integer]
versAutre c
    | c == '£'  = [0x0E, 0x23, 0x0F]
    | c == '°'  = [0x0E, 0x5B, 0x0F]
    | c == 'ç'  = [0x0E, 0x5C, 0x0F]
    | c == '’'  = [0x27]
    | c == '`'  = [0x60]
    | c == '§'  = [0x0E, 0x5D, 0x0F]
    | c == 'à'  = [0x0E, 0x40, 0x0F]
    | c == 'è'  = [0x0E, 0x7F, 0x0F]
    | c == 'é'  = [0x0E, 0x7B, 0x0F]
    | c == 'ù'  = [0x0E, 0x7C, 0x0F]
    | isAscii c = [(fromIntegral . ord) c]
    | otherwise = []
