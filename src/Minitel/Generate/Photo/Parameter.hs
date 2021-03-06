{-|
Module      : Parameter
Description : PhotoVideotex parameter generator
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module allows to generate parameters for photo videotex. All parameters
are not useful with the Minitel.
-}
module Minitel.Generate.Photo.Parameter
    ( mResetToDefault
    , intParm
    , normParm
    , enumParm
    , mFieldLength
    , mFullScreenDisplay
    , mSourceAspectRatio
    , mPhotoAreaLocation
    , mPhotoAreaSize
    , mPicturePlacement
    , mClearPhotoArea
    , mSourcePictureComments
    , mSourcePictureDimensions
    , mSourcePixelDensityStf
    , mSourcePixelDensityVal
    , mSourceSweepDirection
    , mDCImages
    , Component(ComponentRGB, ComponentYCrCb, ComponentCMYK, ComponentY)
    , mSourceComponentDescription
    , mSourceComponentDataPrecision
    , mSourceComponentOrder
    , mSourceLevelAssignmentFix
    , mSourceLevelAssignment
    , JPGHierarchical(NonHierarchical, Hierarchical)
    , JPGTransform(DCT, DPCM)
    , JPGOrder(Sequential, Progressive)
    , JPGCoding(Huffman, Arithmetic)
    , mJPEGCodingMode
    , mEncodingTableManagement
    , mApplicationMarkerCodesAssignment
    , TranslationMode( NoTranslation
                     , NoTranslationExceptUS
                     , Encoding3in4
                     , Shift8bits
                     , Shift7bits
                     , NoTranslationExceptSp
                     )
    , mTranslationModeEncoding
    ) where

import           Minitel.Constants.Photo
import           Minitel.Type.MNatural
import           Minitel.Type.MString

-- | Convert an Int into a field length
mFieldLength :: Int -> MString
mFieldLength i
    | i < 0     = error "Negative number !"
    | i < 32    = [mnat i]
    | otherwise = mnat (0x40 + hi):mFieldLength lo
    where (hi, lo) = divMod i 32

-- * Parameters
--   Parameters are MString composed of
--   - a parameter type
--   - a field length
--   - the parameter value

-- | Boolean parameter
boolParm :: Bool -> MString
boolParm True  = [pBoolean] ++ mFieldLength 1 ++ [0x01]
boolParm False = [pBoolean] ++ mFieldLength 1 ++ [0x00]

-- | Integer parameter
intParm :: Int -> MString
intParm i
    | i < -8192 = error "Number below -8192"
    | i > 8191  = error "Number above 8191"
    | otherwise = [pInteger] ++ mFieldLength 2 ++ [mnat byte1, mnat byte2]
    where cpl            = if i < 0 then 16384 + i else i
          (byte1, byte2) = (div cpl 128, mod cpl 128)

-- | Float parameter
normParm :: Float -> MString
normParm n
    | n < -1    = error "Number below -1"
    | n > 1     = error "Number above 1"
    | otherwise = [pNormalised] ++ mFieldLength 2 ++ [mnat byte1, mnat byte2]
    where cpl   = round $ if n < 0 then 16384 + (1 + n) * 4096 else n * 4096
          (byte1, byte2) = divMod cpl 128

class Enumeration en where
    enumToMNat :: en -> MNat

-- | Enumeration parameter
enumParm :: (Enumeration a) => a -> MString
enumParm i = [pEnumeration] ++ mFieldLength 1 ++ [enumToMNat i]

-- * Parameter Status Attribute

-- | Reset to default values
mResetToDefault :: Bool -> MString
mResetToDefault b = rtd ++ boolParm b

-- * Picture Display Attributes

-- | Full screen display
mFullScreenDisplay :: Bool -> MString
mFullScreenDisplay b = fsd ++ boolParm b

-- | Source aspect ratio
mSourceAspectRatio :: Int -> Int -> MString
mSourceAspectRatio araw arah = asr ++ intParm araw ++ intParm arah

-- | Photo area location
--   The location is given as a percentage of the screen width\/height
mPhotoAreaLocation :: Float -> Float -> MString
mPhotoAreaLocation loch locv = loc ++ normParm loch ++ normParm locv

-- | Photo area size
--   The size is given as a percentage of the screen width\/height
mPhotoAreaSize :: Float -> Float -> MString
mPhotoAreaSize sizw sizh = pas ++ normParm sizw ++ normParm sizh

-- | Picture placement
--   Picture placement inside the photo area
mPicturePlacement :: Int -> Int -> Float -> Float -> MString
mPicturePlacement refh refv offh offv =
    ppl ++ intParm refh ++ intParm refv ++ normParm offh ++ normParm offv

-- | Clear photo area
--   Useful for pictures handling transparency
mClearPhotoArea :: Bool -> MString
mClearPhotoArea b = cpa ++ boolParm b

-- * Source Picture Attributes

-- | Source picture comments
--   TODO: implement source picture comments
mSourcePictureComments :: Int -> Int -> MString
mSourcePictureComments = undefined

-- | Source picture dimensions
mSourcePictureDimensions :: Int -> Int -> MString
mSourcePictureDimensions nph npv = pds ++ intParm nph ++ intParm npv

-- | Density STF
data DensityStf = Density422625
                | Density422525
                | Density211625
                | Density211525
                | DensityCIF
                | DensityNA

instance Enumeration DensityStf where
    enumToMNat Density422625 = 0x01
    enumToMNat Density422525 = 0x02
    enumToMNat Density211625 = 0x03
    enumToMNat Density211525 = 0x04
    enumToMNat DensityCIF    = 0x05
    enumToMNat DensityNA     = 0x06

-- | Density unit
data DensityUnit = PixelsPerInch
                 | PixelsPerCm
                 | PixelsPerMm

instance Enumeration DensityUnit where
    enumToMNat PixelsPerInch = 0x01
    enumToMNat PixelsPerCm   = 0x02
    enumToMNat PixelsPerMm   = 0x03

-- | Source pixel density STF
mSourcePixelDensityStf :: DensityStf -> MString
mSourcePixelDensityStf density = pid ++ enumParm density

-- | Source pixel density Values
mSourcePixelDensityVal :: Int -> Int -> Int -> Int -> DensityUnit -> MString
mSourcePixelDensityVal phnum phden pvnum pvden densityUnit =
    pid ++ intParm phnum ++ intParm phden
        ++ intParm pvnum ++ intParm pvden
        ++ enumParm densityUnit

-- | Vertical sweep
data VerticalSweep   = TopToBottom | BottomToTop

instance Enumeration VerticalSweep where
    enumToMNat TopToBottom = 0x01
    enumToMNat BottomToTop = 0x02

-- | Horizontal sweep
data HorizontalSweep = LeftToRight | RightToLeft

instance Enumeration HorizontalSweep where
    enumToMNat LeftToRight = 0x01
    enumToMNat RightToLeft = 0x02

-- | Source sweep direction
mSourceSweepDirection :: VerticalSweep -> HorizontalSweep -> MString
mSourceSweepDirection sdir sdil = swd ++ enumParm sdir ++ enumParm sdil

-- | DCI inages
mDCImages :: Bool -> MString
mDCImages b = dci ++ boolParm b

-- * Source Signal Attributes

-- | Component
--   The Minitel only supports ComponentY
data Component = ComponentRGB
               | ComponentYCrCb
               | ComponentCMYK
               | ComponentY

instance Enumeration Component where
    enumToMNat ComponentRGB   = 0x01
    enumToMNat ComponentYCrCb = 0x02
    enumToMNat ComponentCMYK  = 0x03
    enumToMNat ComponentY     = 0x04

-- | Source component description
mSourceComponentDescription :: Component -> MString
mSourceComponentDescription com = scd ++ enumParm com

-- | Source component data precision
mSourceComponentDataPrecision :: Int -> MString
mSourceComponentDataPrecision cpt = cdp ++ intParm cpt

-- | Source component order
mSourceComponentOrder :: Int -> MString
mSourceComponentOrder cor = cmo ++ intParm cor

-- | Level assignment
data LevelAssignment = CCIR6011

instance Enumeration LevelAssignment where
    enumToMNat CCIR6011 = 0x01

-- | Source level assignment fix
mSourceLevelAssignmentFix :: MString
mSourceLevelAssignmentFix = las ++ enumParm CCIR6011

-- | Source level assignment
mSourceLevelAssignment :: Int -> Int -> MString
mSourceLevelAssignment low hi = las ++ intParm low ++ intParm hi

-- * Source Coding Algorithm

data JPGHierarchical = NonHierarchical | Hierarchical
data JPGTransform    = DCT | DPCM
data JPGOrder        = Sequential | Progressive
data JPGCoding       = Huffman | Arithmetic

-- | JPEG coding mode
mJPEGCodingMode :: JPGHierarchical -> JPGTransform -> JPGOrder -> JPGCoding
                -> MString
mJPEGCodingMode h t o c = jpg ++ intParm (ih + it + io + ic)
    where ih = case h of NonHierarchical -> 0x00
                         Hierarchical    -> 0x01
          it = case t of DCT             -> 0x00
                         DPCM            -> 0x02
          io = case o of Sequential      -> 0x00
                         Progressive     -> 0x04
          ic = case c of Huffman         -> 0x00
                         Arithmetic      -> 0x08

-- | Table type
data TableType = TableQuantisation
               | TableHuffman

instance Enumeration TableType where
    enumToMNat TableQuantisation = 0x01
    enumToMNat TableHuffman      = 0x02

-- | Table management
data TableManagement = LoadDefaultTable
                     | UseCurrentTable
                     | TableWillBeTransferred

instance Enumeration TableManagement where
    enumToMNat LoadDefaultTable       = 0x01
    enumToMNat UseCurrentTable        = 0x02
    enumToMNat TableWillBeTransferred = 0x03

-- | Encoding table management
mEncodingTableManagement :: TableType -> Int -> TableManagement -> MString
mEncodingTableManagement ttp tid tst =
    etm ++ enumParm ttp ++ intParm tid ++ enumParm tst

-- | Application marker
data ApplicationMarker = AnimatedImages
                       | ColourPaletteDefinition
                       | ToBeAllocated02
                       | ToBeAllocated03
                       | ToBeAllocated04
                       | ToBeAllocated05
                       | ToBeAllocated06
                       | ToBeAllocated07
                       | ToBeAllocated08
                       | ToBeAllocated09
                       | ToBeAllocated0A
                       | ToBeAllocated0B
                       | ToBeAllocated0C
                       | ToBeAllocated0D
                       | ToBeAllocated0E
                       | ToBeAllocated0F

instance Enumeration ApplicationMarker where
    enumToMNat AnimatedImages          = 0x00
    enumToMNat ColourPaletteDefinition = 0x01
    enumToMNat ToBeAllocated02         = 0x02
    enumToMNat ToBeAllocated03         = 0x03
    enumToMNat ToBeAllocated04         = 0x04
    enumToMNat ToBeAllocated05         = 0x05
    enumToMNat ToBeAllocated06         = 0x06
    enumToMNat ToBeAllocated07         = 0x07
    enumToMNat ToBeAllocated08         = 0x08
    enumToMNat ToBeAllocated09         = 0x09
    enumToMNat ToBeAllocated0A         = 0x0A
    enumToMNat ToBeAllocated0B         = 0x0B
    enumToMNat ToBeAllocated0C         = 0x0C
    enumToMNat ToBeAllocated0D         = 0x0D
    enumToMNat ToBeAllocated0E         = 0x0E
    enumToMNat ToBeAllocated0F         = 0x0F

-- | Application marker code assignment
mApplicationMarkerCodesAssignment :: ApplicationMarker -> MString
mApplicationMarkerCodesAssignment mak = ama ++ enumParm mak

-- * Transmission Channel Attributes

-- | Translation mode
data TranslationMode = NoTranslation
                     | NoTranslationExceptUS
                     | Encoding3in4
                     | Shift8bits
                     | Shift7bits
                     | NoTranslationExceptSp

instance Enumeration TranslationMode where
    enumToMNat NoTranslation         = 0x00
    enumToMNat NoTranslationExceptUS = 0x01
    enumToMNat Encoding3in4          = 0x02
    enumToMNat Shift8bits            = 0x03
    enumToMNat Shift7bits            = 0x04
    enumToMNat NoTranslationExceptSp = 0x05

-- | Translation mode encoding
mTranslationModeEncoding :: TranslationMode -> MString
mTranslationModeEncoding tmod = tme ++ enumParm tmod

